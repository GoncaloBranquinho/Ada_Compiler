#!/bin/bash

# Script de testes com validação de resultados
# Compila exemplos Ada e compara com outputs esperados
# Uso: ./testValidation.sh [opções]
#   ./testValidation.sh              # Recompila tudo e executa testes
#   ./testValidation.sh --no-rebuild # Apenas executa testes
#   ./testValidation.sh --clean      # Apaga arquivos de teste

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

# Configuração
BIN_DIR="src/bin"
EXEC="$BIN_DIR/ada"
TEST_DIR="test_cases"

mkdir -p "$TEST_DIR"

# Estatísticas
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
declare -a FAILED_TEST_NAMES



show_help() {
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║           Script de Testes do Compilador Ada                   ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${CYAN}USO:${NC}"
    echo "  ./testValidation.sh [opções] [ficheiro(s)_teste]"
    echo ""
    echo -e "${CYAN}OPÇÕES:${NC}"
    echo "  -e                                Apenas executa testes (sem recompilar)"
    echo "  -c                                Apaga todos os ficheiros de teste temporários"
    echo "  -h                                Mostra esta ajuda"
    echo "  (sem ficheiros nos argumentos)    Executa todos os testes"
    echo "  (com ficheiros nos argumentos)    Executa apenas esses testes"
    echo ""
    echo -e "${CYAN}EXEMPLOS:${NC}"
    echo "  # Executar todos os testes"
    echo "  ./testValidation.sh"
    echo ""
    echo "  # Testar um ficheiro específico sem recompilar"
    echo "  ./testValidation.sh -e test_01_arithmetric.adb"
    exit 0
}



# ============================================================================
# Processar argumentos
# ============================================================================

REBUILD=true
CLEAN_ONLY=false

while [ $# -gt 0 ]; do
    case "$1" in
        -e)
            REBUILD=false
            shift
            ;;
        -c)
            CLEAN_ONLY=true
            shift
            ;;
        -h)
            show_help
            ;;
        -*)
            echo "Argumento desconhecido: $1"
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

# ============================================================================
# Função para limpar arquivos de teste
# ============================================================================
clean_tests() {
   echo -e "${YELLOW}Limpando ficheiros de teste (exceto .adb, .expected e .input)...${NC}"
    if [ -d "$TEST_DIR" ]; then
        rm -f "$TEST_DIR"/*.bin
        rm -f "$TEST_DIR"/*.mips
        rm -f "$TEST_DIR"/*.output
        rm -f "$TEST_DIR"/*.debugging
        echo -e "${GREEN}✓ Ficheiros temporários apagados${NC}"
    else
        echo -e "${YELLOW}Nenhum ficheiro de teste para apagar${NC}"
    fi
    exit 0
}

# ============================================================================
# Função para executar um teste
# ============================================================================

run_test() {
    local name=$1
    local ada_file="$TEST_DIR/${name}.adb"
    local mips_file="$TEST_DIR/${name}.mips"
    local expected_file="$TEST_DIR/${name}.expected"
    local output_file="$TEST_DIR/${name}.output"
    local input_file="$TEST_DIR/${name}.input"

    local error_msg=""

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    # Compila para MIPS
    if ! "$EXEC" "$ada_file" > /dev/null 2>&1; then
        error_msg="COMPILAÇÃO FALHOU"
        echo -e "${RED}✗ $name ($error_msg)${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name ($error_msg)")
        return 1
    fi

    # Verifica se o ficheiro .mips foi criado
    if [ ! -f "$mips_file" ] || [ ! -s "$mips_file" ]; then
        error_msg="NENHUM MIPS GERADO"
        echo -e "${RED}✗ $name ($error_msg)${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name ($error_msg)")
        return 1
    fi

    # Verifica MARS
    if ! command -v mars &> /dev/null && [ ! -f "Mars4_5.jar" ]; then
        error_msg="MARS NÃO ENCONTRADO"
        echo -e "${RED}✗ $name ($error_msg)${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name ($error_msg)")
        return 1
    fi

    # Executa no MARS e captura erros
    if command -v mars &> /dev/null; then
        if [ -f "$input_file" ]; then
            mars_cmd="mars nc \"$mips_file\" < \"$input_file\""
        else
            mars_cmd="mars nc \"$mips_file\""
        fi
    else
        if [ -f "$input_file" ]; then
            mars_cmd="java -jar Mars4_5.jar nc \"$mips_file\" < \"$input_file\""
        else
            mars_cmd="java -jar Mars4_5.jar nc \"$mips_file\""
        fi
    fi

    if ! eval "$mars_cmd" > "$output_file" 2>&1; then
        error_msg="ERRO EXECUÇÃO MARS"
        echo -e "${RED}✗ $name ($error_msg)${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name ($error_msg)")
        return 1
    fi

    # Validação do output
    local expected_output=$(cat "$expected_file" 2>/dev/null || echo "")
    local actual_output=$(cat "$output_file" 2>/dev/null || echo "")
    
    if [[ "$actual_output" != *"$expected_output"* ]]; then
        error_msg="OUTPUT DIFERENTE"
        # Mostra primeiras diferenças para debug
        if [ ${#expected_output} -gt 0 ] && [ ${#actual_output} -gt 0 ]; then
            error_msg+=" (exp: '$expected_output' | obt: '$actual_output')"
        fi
        echo -e "${RED}✗ $name ($error_msg)${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name ($error_msg)")
        return 1
    fi

    echo -e "${GREEN}✓ $name${NC}"
    PASSED_TESTS=$((PASSED_TESTS + 1))
    return 0
}

# ============================================================================
# Verificar dependências
# ============================================================================
check_compiler() {
    if [ ! -f "$EXEC" ]; then
        echo -e "${RED}✗ Compilador não encontrado: $EXEC${NC}"
        exit 1
    fi
}

# ============================================================================
# LÓGICA PRINCIPAL
# ============================================================================

echo -e "\n${BLUE}╔═══════════════════════════════╗${NC}"
echo -e "${BLUE}║    Testes do Compilador Ada   ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════╝${NC}\n"

# Se --clean foi passado, apagar e sair
if [ "$CLEAN_ONLY" = true ]; then
    clean_tests
fi

# Recompilar se necessário
if [ "$REBUILD" = true ]; then
    echo -e "${BLUE}Recompilando...${NC}"
    if make -C src clean 2>&1; then
        if make -C src build 2>&1; then
            echo -e "${GREEN}✓ Recompilação bem-sucedida${NC}"
        else
            echo -e "${RED}✗ Erro na compilação${NC}"
            exit 1
        fi
    else
        echo -e "${RED}✗ Erro ao limpar${NC}"
        exit 1
    fi
    echo ""
fi

check_compiler

# ============================================================================
# Ler e executar testes dos ficheiros
# ============================================================================


if [ $# -gt 0 ]; then
    for arg in "$@"; do
        name=$(basename "$arg" .adb)
        ada_file="$TEST_DIR/${name}.adb"
        expected_file="$TEST_DIR/${name}.expected"

        if [ ! -f "$ada_file" ]; then
            echo -e "${YELLOW}⚠ Ficheiro de teste não encontrado: $ada_file${NC}"
            continue
        fi
        if [ ! -f "$expected_file" ]; then
            echo -e "${YELLOW}⚠ Ficheiro esperado não encontrado: $expected_file${NC}"
            continue
        fi

        run_test "$name"
    done
else

  # Encontra todos os ficheiros .adb em test_cases e executa
  if ls "$TEST_DIR"/*.adb 1> /dev/null 2>&1; then
      for ada_file in "$TEST_DIR"/*.adb; do
          name=$(basename "$ada_file" .adb)
          expected_file="$TEST_DIR/${name}.expected"
          # Verifica se existe o ficheiro expected correspondente
          if [ ! -f "$expected_file" ]; then
              echo -e "${YELLOW}⚠ Ficheiro esperado não encontrado: $expected_file${NC}"
              continue
          fi
          run_test "$name"
      done
  else
      echo -e "${YELLOW}⚠ Nenhum ficheiro de teste encontrado em $TEST_DIR${NC}"
      echo -e "${YELLOW}Cria ficheiros .adb, .expected (e .input se necessário) em test_cases/{{NC}"
      exit 1
  fi
fi

# ============================================================================
# RESUMO
# ============================================================================

echo -e "\n${BLUE}╔════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║         Resumo dos Testes              ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════╝${NC}\n"

echo -e "${CYAN}Total de Testes:${NC} $TOTAL_TESTS"
echo -e "${GREEN}Testes Passaram:${NC} $PASSED_TESTS"
echo -e "${RED}Testes Falharam:${NC} $FAILED_TESTS"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "\n${GREEN}╔════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║    🎉 TODOS OS TESTES PASSARAM! 🎉     ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════╝${NC}\n"
    exit 0
else
    echo -e "\n${RED}╔════════════════════════════════════════╗${NC}"
    echo -e "${RED}║     ❌ Alguns testes falharam ❌       ║${NC}"
    echo -e "${RED}╚════════════════════════════════════════╝${NC}\n"
    echo -e "${RED}Testes que falharam:${NC}"
    for test in "${FAILED_TEST_NAMES[@]}"; do
        echo -e "  ${RED}✗ $test${NC}"
    done
    echo ""
    echo -e "${YELLOW}Ficheiros de teste disponíveis em:${NC} $TEST_DIR/"
    echo -e "${YELLOW}Para fazer debugging a um teste:${NC}"
    echo "  cat $TEST_DIR/test_XX_nome.adb                  # Ver código Ada"
    echo "  cat $TEST_DIR/test_XX_nomeAST.debugging         # Ver AST gerada"
    echo "  cat $TEST_DIR/test_XX_nomeTable.debugging       # Ver Tabela de Símbolos"
    echo "  cat $TEST_DIR/test_XX_nomeIR.debugging          # Ver Código Intermédio"
    echo "  cat $TEST_DIR/test_XX_nomeIROptimized.debugging # Ver Código Intermédio Otimizado"
    echo "  cat $TEST_DIR/test_XX_nomeAllocation.expected   # Ver alocação de memória"
    echo "  cat $TEST_DIR/test_XX_nome.mips                 # Ver MIPS gerado"
    echo "  cat $TEST_DIR/test_XX_nome.output               # Ver output do MARS"
    echo "  cat $TEST_DIR/test_XX_nome.expected             # Ver output esperado"
    echo ""
    exit 1
fi

