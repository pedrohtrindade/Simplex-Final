import sys #pra ler o arquivo
import os
import random

def ler_arquivo(nome):
    with open(nome, "r") as f:
        linhas = []
        for linha in f:
            linha = linha.strip()
            if linha != "":
                linhas.append(linha)
    return linhas


def parsear_expressao(expr):
    expr = expr.replace(" ", "")
    expr = expr.replace("-", "+-") #troca o sinal para depois poder separar
    termos = expr.split("+")
    coefs = {}
    for termo in termos:
        termo = termo.strip()
        if termo == "":
            continue
        if "x" in termo:
            partes = termo.split("x") #divide coeficiente e indice
            if partes[0] == "" or partes[0] == "+":
                coef = 1
            elif partes[0] == "-":
                coef = -1
            else:
                coef = float(partes[0])
            var = "x" + partes[1]
            coefs[var] = coef
    return coefs


def eh_nao_negatividade(linha):
    if ">=" not in linha:
        return False
    partes = linha.split(">=")
    coefs = parsear_expressao(partes[0])
    b = float(partes[1])
    if len(coefs) == 1 and b == 0:
        for v in coefs:
            if coefs[v] == 1:
                return True
    return False


def parsear_problema(linhas):
    variaveis = []
    restricoes = []
    c = {} #coeficientes da função objetivo
    tipo = ""
    for linha in linhas:
        # função objetivo
        if "max" in linha.lower() or "min" in linha.lower():
            if "max" in linha.lower():
                tipo = "max"
            else:
                tipo = "min"
            expr = linha.split("=")[1]
            c = parsear_expressao(expr)
            for v in c:
                if v not in variaveis:
                    variaveis.append(v)
        elif eh_nao_negatividade(linha):
            coefs = parsear_expressao(linha.split(">=")[0])
            for v in coefs:
                if v not in variaveis:
                    variaveis.append(v)
        # restrições
        else:
            if ">=" in linha:
                partes = linha.split(">=")
                sinal = ">="
            elif "<=" in linha:
                partes = linha.split("<=")
                sinal = "<="
            else:
                partes = linha.split("=")
                sinal = "="
            coefs = parsear_expressao(partes[0])
            b = float(partes[1])
            restricoes.append((coefs, sinal, b))
            for v in coefs:
                if v not in variaveis:
                    variaveis.append(v)
    variaveis.sort()
    return tipo, c, restricoes, variaveis


def forma_padrao(tipo, c, restricoes, variaveis):
    A = [] #matriz dos coeficientes
    b = [] #resultados
    nomes = variaveis[:]
    extras = 0
    # conta quantas extras existirão
    for coefs, sinal, rhs in restricoes:
        if sinal == "<=" or sinal == ">=":
            extras += 1
    total = len(variaveis) + extras
    extra_atual = 0
    for coefs, sinal, rhs in restricoes:
        linha = []
        # variáveis originais
        for v in variaveis:
            linha.append(coefs.get(v, 0))
        # zeros das extras
        for i in range(extras):
            linha.append(0)
        # coloca folga/excesso
        if sinal == "<=":
            linha[len(variaveis) + extra_atual] = 1
            nomes.append("s" + str(extra_atual+1))
            extra_atual += 1
        elif sinal == ">=":
            linha[len(variaveis) + extra_atual] = -1
            nomes.append("e" + str(extra_atual+1))
            extra_atual += 1
        A.append(linha)
        b.append(rhs)
    # vetor custo
    custo = []
    for nome in nomes:
        if nome in c:
            valor = c[nome] #pega o coeficiente da função objetivo
            if tipo == "max":
                valor *= -1 #multiplica por -1 pra deixar como se fosse min
            custo.append(valor)
        else:
            custo.append(0)
    return A, b, nomes, custo #custo = vetor da função objetivo


def trocar_sinal(sinal):
    if sinal == ">=":
        return "<="
    if sinal == "<=":
        return ">="
    return "="


def adicionar_coluna(A):
    for i in range(len(A)):
        A[i].append(0)


def forma_padrao_fase1(tipo, c, restricoes, variaveis):
    A = []
    b = []
    nomes = variaveis[:]
    B = []
    N = []
    artificiais = []
    cont_folga = 1
    cont_excesso = 1

    for coefs, sinal, rhs in restricoes:
        if rhs < 0:
            novos = {}
            for v in coefs:
                novos[v] = -coefs[v]
            coefs = novos
            rhs = -rhs
            sinal = trocar_sinal(sinal)

        linha = []
        for v in variaveis:
            linha.append(coefs.get(v, 0))
        for i in range(len(nomes) - len(variaveis)):
            linha.append(0)
        A.append(linha)
        b.append(rhs)
        pos_linha = len(A) - 1

        if sinal == "<=":
            adicionar_coluna(A)
            nomes.append("s" + str(cont_folga))
            A[pos_linha][len(nomes) - 1] = 1
            B.append(len(nomes) - 1)
            cont_folga += 1
        elif sinal == ">=":
            adicionar_coluna(A)
            nomes.append("e" + str(cont_excesso))
            A[pos_linha][len(nomes) - 1] = -1
            cont_excesso += 1

            adicionar_coluna(A)
            nomes.append("a" + str(len(artificiais) + 1))
            A[pos_linha][len(nomes) - 1] = 1
            B.append(len(nomes) - 1)
            artificiais.append(len(nomes) - 1)
        else:
            adicionar_coluna(A)
            nomes.append("a" + str(len(artificiais) + 1))
            A[pos_linha][len(nomes) - 1] = 1
            B.append(len(nomes) - 1)
            artificiais.append(len(nomes) - 1)

    for j in range(len(nomes)):
        if j not in B:
            N.append(j)

    custo_fase1 = []
    custo_fase2 = []
    for j in range(len(nomes)):
        if j in artificiais:
            custo_fase1.append(1)
        else:
            custo_fase1.append(0)

        if nomes[j] in c:
            valor = c[nomes[j]]
            if tipo == "max":
                valor *= -1
            custo_fase2.append(valor)
        else:
            custo_fase2.append(0)

    return A, b, nomes, custo_fase1, custo_fase2, B, N, artificiais


def transposta(M): # M = matriz original
    T = []
    for j in range(len(M[0])): #percorre as colunas da matriz original
        linha = []
        for i in range(len(M)):# o mesmo com as linhas
            linha.append(M[i][j])
        T.append(linha)
    return T


def produto_interno(a, b):
    soma = 0
    for i in range(len(a)):
        soma += a[i] * b[i]
    return soma

def resolver_sistema(A, b): #Gauss
    n = len(b)
    tentativas = 0
    while tentativas < 100: # conta de probabilidade 5!/3!(5-3!)
        sistema = []
        for i in range(n):
            sistema.append((A[i][:], b[i])) #junta o sistema para depois embaralhar
        random.shuffle(sistema)
        M = []
        for linha, valor in sistema:
            M.append(linha + [valor])
        erro = False
        for i in range(n):
            if M[i][i] == 0: #pivo
                erro = True
                break
            pivo = M[i][i]
            for j in range(i, n+1):
                M[i][j] /= pivo #divide a linha
            for k in range(n):
                if k != i:
                    fator = M[k][i]
                    for j in range(i, n+1):
                        M[k][j] -= fator * M[i][j] #eliminação
        if not erro:
            return [M[i][n] for i in range(n)]
        tentativas += 1
    return None


def montar_matriz_basica(A, B):
    m = len(A)
    Bmat = []
    for i in range(m):
        linha = []
        for j in B:
            linha.append(A[i][j])
        Bmat.append(linha)
    return Bmat


def pegar_coluna(A, j):
    coluna = []
    for i in range(len(A)):
        coluna.append(A[i][j])
    return coluna


def simplex_com_base(A, b, nomes, custo, B, N):
    m = len(A)
    n = len(A[0])
    B = B[:]
    N = N[:]
    Bmat = montar_matriz_basica(A, B)
    xB = resolver_sistema(Bmat, b)
    if xB == None:
        return "erro"

    while True:
        Bmat = montar_matriz_basica(A, B)
        cB = []
        for j in B:
            cB.append(custo[j])

        Bt = transposta(Bmat)
        lam = resolver_sistema(Bt, cB)
        if lam == None:
            return "erro"

        custos = []
        for j in N:
            coluna = pegar_coluna(A, j)
            chat = custo[j] - produto_interno(lam, coluna)
            custos.append(chat)

        menor = min(custos)

        if menor >= -0.0000001:
            x = []
            for i in range(n):
                x.append(0)
            for i in range(m):
                x[B[i]] = xB[i]

            z = 0
            for i in range(n):
                z += custo[i] * x[i]

            return B, N, xB, x, z

        indice = custos.index(menor)
        k = N[indice]

        coluna = pegar_coluna(A, k)
        y = resolver_sistema(Bmat, coluna)
        if y == None:
            return "erro"

        menor_razao = 999999
        sai = -1
        for i in range(m):
            if y[i] > 0.0000001:
                razao = xB[i] / y[i]
                if razao < menor_razao:
                    menor_razao = razao
                    sai = i

        if sai == -1:
            return "ilimitado"

        for i in range(m):
            if i == sai:
                xB[i] = menor_razao
            else:
                xB[i] = xB[i] - menor_razao * y[i]

        N[N.index(k)] = B[sai]
        B[sai] = k


def tirar_artificiais_da_base(A, b, B, N, artificiais):
    for i in range(len(B)):
        if B[i] in artificiais:
            Bmat = montar_matriz_basica(A, B)
            for j in N:
                if j not in artificiais:
                    coluna = pegar_coluna(A, j)
                    y = resolver_sistema(Bmat, coluna)
                    if y != None and abs(y[i]) > 0.0000001:
                        N[N.index(j)] = B[i]
                        B[i] = j
                        break
    return B, N



def fase1(A, b, nomes, custo_fase1, B, N, artificiais):
    resultado = simplex_com_base(A, b, nomes, custo_fase1, B, N)
    if resultado == None or resultado == "erro" or resultado == "ilimitado":
        print("Problema infactivel.")
        return None

    B, N, xB, x, z = resultado

    if z > 0.0000001:
        print("Problema infactivel.")
        return None

    B, N = tirar_artificiais_da_base(A, b, B, N, artificiais)

    novo_N = []
    for j in N:
        if j not in artificiais:
            novo_N.append(j)
    N = novo_N

    return B, N


def fase2(A, b, nomes, custo_fase2, B, N, tipo):
    resultado = simplex_com_base(A, b, nomes, custo_fase2, B, N)
    if resultado == "ilimitado":
        print("Problema ilimitado.")
        return
    if resultado == None or resultado == "erro":
        print("Erro ao resolver o problema.")
        return

    B, N, xB, x, z = resultado

    print('\nX e variaveis de folga:')
    for i in range(len(nomes)):
        if nomes[i][0] != "a":
            print(nomes[i], "=", x[i])

    if tipo == "max":
        z *= -1

    print("\nValor de z:")
    print(z)


def simplex_fase2(A, b, nomes, custo):
    m = len(A) #num de restrições
    n = len(A[0])
    B = []
    N = []
    # encontra base inicial
    for j in range(n-m, n):
        B.append(j)
    for j in range(n-m):
        N.append(j)
    xB = b[:]
    while True:
        # monta matriz básica
        Bmat = []
        for i in range(m):
            linha = []
            for j in B:
                linha.append(A[i][j])
            Bmat.append(linha)
        # lambda
        cB = []
        for j in B:
            cB.append(custo[j])
        Bt = transposta(Bmat)
        print("Matriz transposta:")
        for linha in Bt:
            print(linha)
        lam = resolver_sistema(Bt, cB)
        # custos relativos
        custos = []
        for j in N:
            coluna = []
            for i in range(m):
                coluna.append(A[i][j])
            chat = custo[j] - produto_interno(lam, coluna)
            custos.append(chat)
        menor = min(custos)
        # ótimo
        if menor >= 0:
            print("Matriz básica nova:")
            for i in range(m):
                linha = []
                for j in B:
                    linha.append(A[i][j])
                print(linha)
            print("\nMatriz não básica nova:")
            for i in range(m):
                linha = []
                for j in N:
                    linha.append(A[i][j])
                print(linha)

            x = []
            for i in range(n):
                x.append(0)
            for i in range(m):
                x[B[i]] = xB[i] #coloca valores básicos
            print('\nX e variáveis de folga:')
            for i in range(n):
                print(nomes[i], "=", x[i])
            z = 0
            for i in range(n):
                z+= custo[i]*x[i] 
            z *= -1 #desfaz sinal se era max
            print("\nValor de z:")
            print(z)
            return
        # entra
        indice = custos.index(menor)
        k = N[indice]
        # direção simplex
        coluna = []
        for i in range(m):
            coluna.append(A[i][k])
        y = resolver_sistema(Bmat, coluna)
        # razão mínima, serve pra decidir qual sai da basee 
        menor_razao = 999999
        sai = -1
        for i in range(m):
            if y[i] > 0:
                razao = xB[i] / y[i]
                if razao < menor_razao:
                    menor_razao = razao
                    sai = i
        if sai == -1:
            return
        # atualiza xB
        for i in range(m):
            if i == sai:
                xB[i] = menor_razao
            else:
                xB[i] = xB[i] - menor_razao * y[i]
        # troca base
        N[N.index(k)] = B[sai]
        B[sai] = k
def main():
    if len(sys.argv) > 1:
        nome_arquivo = sys.argv[1]
    else:
        pasta = os.path.dirname(__file__)
        nome_arquivo = os.path.join(pasta, "problema.txt")

    linhas = ler_arquivo(nome_arquivo)
    tipo, c, restricoes, variaveis = parsear_problema(linhas)
    A, b, nomes, custo_fase1, custo_fase2, B, N, artificiais = forma_padrao_fase1(
        tipo,
        c,
        restricoes,
        variaveis
    )
#decisão do simplex
    if len(artificiais) > 0:
        resultado = fase1(A, b, nomes, custo_fase1, B, N, artificiais)
        if resultado == None:
            return
        B, N = resultado

    fase2(A, b, nomes, custo_fase2, B, N, tipo)
if __name__ == "__main__":
    main()
