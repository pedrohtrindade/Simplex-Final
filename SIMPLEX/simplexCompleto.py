import sys #pra ler o arquivo
import os


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
    expr = expr.replace(",", ".")
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
    b = float(partes[1].replace(",", "."))
    if len(coefs) == 1 and b == 0:
        for v in coefs:
            if coefs[v] == 1:
                return True
    return False


def parsear_problema(linhas):
    variaveis = []
    restricoes = []
    nao_negativas = []
    c = {} #coeficientes da funcao objetivo
    tipo = ""
    for linha in linhas:
        # funcao objetivo
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
        # x1 >= 0, x2 >= 0 etc. nao entram como restricoes
        elif eh_nao_negatividade(linha):
            coefs = parsear_expressao(linha.split(">=")[0])
            for v in coefs:
                if v not in variaveis:
                    variaveis.append(v)
                if v not in nao_negativas:
                    nao_negativas.append(v)
        # restricoes
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
            b = float(partes[1].replace(",", "."))
            restricoes.append((coefs, sinal, b))
            for v in coefs:
                if v not in variaveis:
                    variaveis.append(v)
    variaveis.sort()
    return tipo, c, restricoes, variaveis, nao_negativas


def tem_variavel_sem_nao_negatividade(variaveis, nao_negativas):
    for v in variaveis:
        if v not in nao_negativas:
            return True
    return False


def trocar_sinal(sinal):
    if sinal == ">=":
        return "<="
    if sinal == "<=":
        return ">="
    return "="


def adicionar_coluna(A):
    for i in range(len(A)):
        A[i].append(0)


def forma_padrao(tipo, c, restricoes, variaveis):
    A = [] #matriz dos coeficientes
    b = [] #resultados
    nomes = variaveis[:]
    B = [] #indices basicos
    N = []
    artificiais = []
    cont_folga = 1
    cont_excesso = 1

    for coefs, sinal, rhs in restricoes:
        # deixa o lado direito positivo
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
            cont_folga += 1
            A[pos_linha][len(nomes) - 1] = 1
            B.append(len(nomes) - 1)
        elif sinal == ">=":
            adicionar_coluna(A)
            nomes.append("e" + str(cont_excesso))
            cont_excesso += 1
            A[pos_linha][len(nomes) - 1] = -1

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
                valor *= -1 #transforma max em min
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
    M = []
    for i in range(n):
        M.append(A[i][:] + [b[i]])

    for i in range(n):
        pivo_linha = i
        for k in range(i, n):
            if abs(M[k][i]) > abs(M[pivo_linha][i]):
                pivo_linha = k

        if abs(M[pivo_linha][i]) < 0.0000001:
            return None

        aux = M[i]
        M[i] = M[pivo_linha]
        M[pivo_linha] = aux

        pivo = M[i][i]
        for j in range(i, n+1):
            M[i][j] /= pivo #divide a linha

        for k in range(n):
            if k != i:
                fator = M[k][i]
                for j in range(i, n+1):
                    M[k][j] -= fator * M[i][j] #eliminacao

    return [M[i][n] for i in range(n)]


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


def simplex(A, b, nomes, custo, B, N, mostrar):
    m = len(A) #num de restricoes
    n = len(A[0])
    iteracao = 1
    B = B[:]
    N = N[:]

    Bmat = montar_matriz_basica(A, B)
    xB = resolver_sistema(Bmat, b)
    if xB == None:
        return "erro"

    while True:
        if mostrar:
            print("\nIteracao", iteracao)
            print("Base B:")
            for j in B:
                print(nomes[j], end=" ")
            print("\nNao base N:")
            for j in N:
                print(nomes[j], end=" ")
            print()

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

        # otimo
        if menor >= -0.0000001:
            x = []
            for i in range(n):
                x.append(0)
            for i in range(m):
                x[B[i]] = xB[i] #coloca valores basicos

            z = 0
            for i in range(n):
                z += custo[i] * x[i]

            return B, N, xB, x, z

        # entra
        indice = custos.index(menor)
        k = N[indice]

        # direcao simplex
        coluna = pegar_coluna(A, k)
        y = resolver_sistema(Bmat, coluna)
        if y == None:
            return "erro"

        # razao minima, serve pra decidir qual sai da base
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

        # atualiza xB
        for i in range(m):
            if i == sai:
                xB[i] = menor_razao
            else:
                xB[i] = xB[i] - menor_razao * y[i]

        # troca base
        N[N.index(k)] = B[sai]
        B[sai] = k
        iteracao += 1


def tirar_artificiais_da_base(A, b, B, N, artificiais):
    for i in range(len(B)):
        if B[i] in artificiais:
            Bmat = montar_matriz_basica(A, B)
            trocou = False
            for j in N:
                if j not in artificiais:
                    coluna = pegar_coluna(A, j)
                    y = resolver_sistema(Bmat, coluna)
                    if y != None and abs(y[i]) > 0.0000001:
                        N[N.index(j)] = B[i]
                        B[i] = j
                        trocou = True
                        break
            if not trocou:
                trocou = False
    return B, N


def rodar_fase1(A, b, nomes, custo_fase1, B, N, artificiais):
    resultado = simplex(A, b, nomes, custo_fase1, B, N, False)
    if resultado == None or resultado == "erro" or resultado == "ilimitado":
        print("Problema inviavel.")
        return None

    B, N, xB, x, z = resultado

    if z > 0.0000001:
        print("Problema inviavel.")
        return None

    B, N = tirar_artificiais_da_base(A, b, B, N, artificiais)

    # variaveis artificiais nao devem entrar de novo na Fase 2
    novo_N = []
    for j in N:
        if j not in artificiais:
            novo_N.append(j)
    N = novo_N

    return B, N


def rodar_fase2(A, b, nomes, custo_fase2, B, N, tipo, artificiais):
    resultado = simplex(A, b, nomes, custo_fase2, B, N, False)
    if resultado == "ilimitado":
        print("Problema ilimitado.")
        return
    if resultado == None or resultado == "erro":
        print("Problema inviavel.")
        return

    B, N, xB, x, z = resultado

    for i in range(len(nomes)):
        if nomes[i][0] == "x":
            print(nomes[i], "=", x[i])

    if tipo == "max":
        z *= -1 #desfaz o sinal do max

    print("Z =", z)


def main():
    if len(sys.argv) > 1:
        nome_arquivo = sys.argv[1]
    else:
        nome_arquivo = "problema.txt"
        if not os.path.exists(nome_arquivo):
            pasta = os.path.dirname(__file__)
            nome_arquivo = os.path.join(pasta, "problema.txt")
        if not os.path.exists(nome_arquivo):
            print("Arquivo problema.txt nao encontrado.")
            return

    linhas = ler_arquivo(nome_arquivo)
    tipo, c, restricoes, variaveis, nao_negativas = parsear_problema(linhas)

    if tem_variavel_sem_nao_negatividade(variaveis, nao_negativas):
        print("Problema ilimitado.")
        return

    A, b, nomes, custo_fase1, custo_fase2, B, N, artificiais = forma_padrao(
        tipo,
        c,
        restricoes,
        variaveis
    )

    if len(artificiais) > 0:
        resultado = rodar_fase1(A, b, nomes, custo_fase1, B, N, artificiais)
        if resultado == None:
            return
        B, N = resultado

    rodar_fase2(A, b, nomes, custo_fase2, B, N, tipo, artificiais)


if __name__ == "__main__":
    main()
