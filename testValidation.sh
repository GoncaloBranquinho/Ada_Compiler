#!/bin/bash

# Script de testes com validação de resultados
# Compila exemplos Ada e compara com outputs esperados

set -e

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

# ============================================================================
# Função para registar um teste
# ============================================================================
register_test() {
    local name=$1
    local ada_code=$2
    local expected_output=$3
    local ada_file="$TEST_DIR/${name}.ada"
    echo "$ada_code" > "$ada_file"
    echo "$expected_output" > "$TEST_DIR/${name}.expected"
}

# ============================================================================
# Função para executar um teste
# ============================================================================
run_test() {
   local name=$1
    local ada_file="$TEST_DIR/${name}.ada"
    local mips_file="$TEST_DIR/${name}.mips"
    local bin_file="$TEST_DIR/${name}.bin"
    local expected_file="$TEST_DIR/${name}.expected"
    local output_file="$TEST_DIR/${name}.output"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    # Compila para MIPS
    if ! "$EXEC" "$ada_file" > /dev/null 2>&1; then
        echo -e "${RED}✗ $name${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name")
        return 1
    fi

    # Verifica se o ficheiro .bin foi criado
    if [ ! -f "$bin_file" ] || [ ! -s "$bin_file" ]; then
        echo -e "${RED}✗ $name${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name")
        return 1
    fi

    # Copia o ficheiro .bin para .mips
    cp "$bin_file" "$mips_file" 2>/dev/null

    if [ ! -f "$mips_file" ]; then
        echo -e "${RED}✗ $name${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name")
        return 1
    fi

    # Executa no MARS
    if command -v mars &> /dev/null; then
        mars_cmd="mars"
    elif [ -f "Mars4_5.jar" ]; then
        mars_cmd="java -jar Mars4_5.jar"
    else
        echo -e "${RED}✗ $name${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name")
        return 1
    fi

    # Executa e captura output
    eval "$mars_cmd nc \"$mips_file\" 2>&1" > "$output_file"

    # Validação
    if grep -q "$(cat $expected_file)" "$output_file" 2>/dev/null; then
        echo -e "${GREEN}✓ $name${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo -e "${RED}✗ $name${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$name")
        return 1
    fi
}

# ============================================================================
# Verificar dependências
# ============================================================================
check_compiler() {
    if [ ! -f "$EXEC" ]; then
        echo -e "${RED}✗ Compilador não encontrado: $EXEC${NC}"
        echo -e "${YELLOW}Compilar com: make build${NC}"
        exit 1
    fi
    echo -e "${GREEN}✓ Compilador encontrado${NC}"
}

# ============================================================================
# TESTES
# ============================================================================

echo -e "\n${BLUE}╔════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   Suite de Testes do Compilador Ada   ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════╝${NC}\n"

check_compiler

# ─────────────────────────────────────────────────────────────────────────
# Teste 1: Aritmética Básica
# ─────────────────────────────────────────────────────────────────────────
register_test "test_01_arithmetic" \
'procedure Main is
    x : Integer := 10;
    y : Integer := 5;
    z : Integer;
begin
    z := x + y;
    Put_Line(str(z));
end Main;' \
'15'

run_test "test_01_arithmetic"

# ─────────────────────────────────────────────────────────────────────────
# Teste 2: Subtração
# ─────────────────────────────────────────────────────────────────────────
register_test "test_02_subtraction" \
'procedure Main is
    x : Integer := 20;
    y : Integer := 7;
    z : Integer;
begin
    z := x - y;
    Put_Line(str(z));
end Main;' \
'13'

run_test "test_02_subtraction"

# ─────────────────────────────────────────────────────────────────────────
# Teste 3: Multiplicação
# ─────────────────────────────────────────────────────────────────────────
register_test "test_03_multiplication" \
'procedure Main is
    x : Integer := 6;
    y : Integer := 7;
    z : Integer;
begin
    z := x * y;
    Put_Line(str(z));
end Main;' \
'42'

run_test "test_03_multiplication"

# ─────────────────────────────────────────────────────────────────────────
# Teste 4: Divisão
# ─────────────────────────────────────────────────────────────────────────
register_test "test_04_division" \
'procedure Main is
    x : Integer := 24;
    y : Integer := 4;
    z : Integer;
begin
    z := x / y;
    Put_Line(str(z));
end Main;' \
'6'

run_test "test_04_division"

# ─────────────────────────────────────────────────────────────────────────
# Teste 5: Potenciação
# ─────────────────────────────────────────────────────────────────────────
register_test "test_05_power" \
'procedure Main is
    base : Integer := 2;
    exp : Integer := 8;
    resultado : Integer;
begin
    resultado := base ** exp;
    Put_Line(str(resultado));
end Main;' \
'256'

run_test "test_05_power"

# ─────────────────────────────────────────────────────────────────────────
# Teste 6: If-Then-Else (Verdadeiro)
# ─────────────────────────────────────────────────────────────────────────
register_test "test_06_if_true" \
'procedure Main is
    x : Integer := 15;
    resultado : Integer;
begin
    if x > 10 then
        resultado := 1;
    else
        resultado := 0;
    end if;
    Put_Line(str(resultado));
end Main;' \
'1'

run_test "test_06_if_true"

# ─────────────────────────────────────────────────────────────────────────
# Teste 7: If-Then-Else (Falso)
# ─────────────────────────────────────────────────────────────────────────
register_test "test_07_if_false" \
'procedure Main is
    x : Integer := 5;
    resultado : Integer;
begin
    if x > 10 then
        resultado := 1;
    else
        resultado := 0;
    end if;
    Put_Line(str(resultado));
end Main;' \
'0'

run_test "test_07_if_false"

# ─────────────────────────────────────────────────────────────────────────
# Teste 8: While Loop - Soma de 1 a 5
# ─────────────────────────────────────────────────────────────────────────
register_test "test_08_while_sum" \
'procedure Main is
    i : Integer := 1;
    soma : Integer := 0;
begin
    while i <= 5 loop
        soma := soma + i;
        i := i + 1;
    end loop;
    Put_Line(str(soma));
end Main;' \
'15'

run_test "test_08_while_sum"

# ─────────────────────────────────────────────────────────────────────────
# Teste 9: While Loop - Contagem Decrescente
# ─────────────────────────────────────────────────────────────────────────
register_test "test_09_while_countdown" \
'procedure Main is
    i : Integer := 5;
    resultado : Integer := 0;
begin
    while i > 0 loop
        resultado := i;
        i := i - 1;
    end loop;
    Put_Line(str(resultado));
end Main;' \
'1'

run_test "test_09_while_countdown"

# ─────────────────────────────────────────────────────────────────────────
# Teste 10: Fatorial de 5
# ─────────────────────────────────────────────────────────────────────────
register_test "test_10_factorial" \
'procedure Main is
    n : Integer := 5;
    resultado : Integer := 1;
    i : Integer := 1;
begin
    while i <= n loop
        resultado := resultado * i;
        i := i + 1;
    end loop;
    Put_Line(str(resultado));
end Main;' \
'120'

run_test "test_10_factorial"

# ─────────────────────────────────────────────────────────────────────────
# Teste 11: Operações Booleanas - AND
# ─────────────────────────────────────────────────────────────────────────
register_test "test_11_boolean_and" \
'procedure Main is
    a : Boolean := True;
    b : Boolean := True;
    resultado : Boolean;
begin
    resultado := a and b;
    if resultado then
        Put_Line("Verdadeiro");
    else
        Put_Line("Falso");
    end if;
end Main;' \
'Verdadeiro'

run_test "test_11_boolean_and"

# ─────────────────────────────────────────────────────────────────────────
# Teste 12: Operações Booleanas - OR
# ─────────────────────────────────────────────────────────────────────────
register_test "test_12_boolean_or" \
'procedure Main is
    a : Boolean := True;
    b : Boolean := False;
    resultado : Boolean;
begin
    resultado := a or b;
    if resultado then
        Put_Line("Verdadeiro");
    else
        Put_Line("Falso");
    end if;
end Main;' \
'Verdadeiro'

run_test "test_12_boolean_or"

# ─────────────────────────────────────────────────────────────────────────
# Teste 13: Comparação - Igualdade
# ─────────────────────────────────────────────────────────────────────────
register_test "test_13_equality" \
'procedure Main is
    x : Integer := 42;
    y : Integer := 42;
    resultado : Boolean;
begin
    resultado := x = y;
    if resultado then
        Put_Line("Igual");
    else
        Put_Line("Diferente");
    end if;
end Main;' \
'Igual'

run_test "test_13_equality"

# ─────────────────────────────────────────────────────────────────────────
# Teste 14: Comparação - Menor Que
# ─────────────────────────────────────────────────────────────────────────
register_test "test_14_less_than" \
'procedure Main is
    x : Integer := 10;
    y : Integer := 20;
    resultado : Boolean;
begin
    resultado := x < y;
    if resultado then
        Put_Line("Menor");
    else
        Put_Line("Não menor");
    end if;
end Main;' \
'Menor'

run_test "test_14_less_than"

# ─────────────────────────────────────────────────────────────────────────
# Teste 15: Comparação - Menor ou Igual
# ─────────────────────────────────────────────────────────────────────────
register_test "test_15_less_equal" \
'procedure Main is
    x : Integer := 10;
    y : Integer := 10;
    resultado : Boolean;
begin
    resultado := x <= y;
    if resultado then
        Put_Line("Menor ou igual");
    else
        Put_Line("Maior");
    end if;
end Main;' \
'Menor ou igual'

run_test "test_15_less_equal"

# ============================================================================
# RESUMO
# ============================================================================

echo -e "\n${BLUE}╔════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║         Resumo dos Testes             ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════╝${NC}\n"

echo -e "${CYAN}Total de Testes:${NC} $TOTAL_TESTS"
echo -e "${GREEN}Testes Passaram:${NC} $PASSED_TESTS"
echo -e "${RED}Testes Falharam:${NC} $FAILED_TESTS"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "\n${GREEN}╔════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║    🎉 TODOS OS TESTES PASSARAM! 🎉    ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════╝${NC}\n"
    exit 0
else
    echo -e "\n${RED}╔════════════════════════════════════════╗${NC}"
    echo -e "${RED}║     ❌ Alguns testes falharam ❌      ║${NC}"
    echo -e "${RED}╚════════════════════════════════════════╝${NC}\n" 
    echo -e "${YELLOW}Ficheiros de teste disponíveis em:${NC} $TEST_DIR/"
    echo -e "${YELLOW}Para debugar um teste:${NC}"
    echo "  cat $TEST_DIR/test_XX_nome.ada       # Ver código Ada"
    echo "  cat $TEST_DIR/test_XX_nome.mips      # Ver MIPS gerado"
    echo "  cat $TEST_DIR/test_XX_nome.output    # Ver output do MARS"
    echo "  cat $TEST_DIR/test_XX_nome.expected  # Ver output esperado"
    echo ""
    exit 1
fi
