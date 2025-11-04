# Passeio do Cavalo (Haskell)

## Descrição
Este projeto implementa uma variante do problema do **Passeio do Cavalo** no xadrez.  
---

## Instalação do Ambiente Haskell no Windows

### 1. Instalar GHCup
GHCup gerencia todas as ferramentas Haskell (GHC, Cabal, HLS):

1. Acesse: [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/)  
2. Baixe e execute o instalador.  
3. Durante a instalação, aceite instalar:
   - GHC (compilador Haskell)
   - Cabal (gerenciador de pacotes)
   - Haskell Language Server (HLS)
   - MSys2 (para compatibilidade no Windows)

### 2. Verificar instalações
Abra o PowerShell e rode:

ghc --version
cabal --version
haskell-language-server-wrapper-2.10.0.0.exe --version

### 3. Configurar PATH (se necessário)
Se o HLS não for reconhecido, adicione a pasta do GHCup ao PATH:

C:\ghcup\bin

No Windows:
1. Abra **Painel de Controle → Sistema → Configurações avançadas do sistema → Variáveis de Ambiente**  
2. Selecione **Path → Editar → Novo**  
3. Adicione o caminho acima e clique **OK**  

### 4. Testar ambiente
Abra um novo PowerShell e rode:

haskell-language-server-wrapper-2.10.0.0.exe --version

Se mostrar a versão, seu ambiente está pronto para executar projetos Haskell.

---

## Como rodar o projeto

1. Navegue até o diretório do projeto:

cd caminho/para/Passeio-do-Cavalo

2. Execute o programa com:

cabal run Passeio-do-Cavalo -- entrada.txt

O programa irá processar cada linha do arquivo e mostrar o resultado no terminal.
