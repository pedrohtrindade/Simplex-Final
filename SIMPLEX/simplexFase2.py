import sys #pra ler o arquivo
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
    linhas = ler_arquivo(sys.argv[1])
    tipo, c, restricoes, variaveis = parsear_problema(linhas)
    A, b, nomes, custo = forma_padrao(
        tipo,
        c,
        restricoes,
        variaveis
    )
    simplex_fase2(A, b, nomes, custo)
if __name__ == "__main__":
    main()