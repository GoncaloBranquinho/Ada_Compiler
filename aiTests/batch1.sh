#!/usr/bin/env bash

mkdir -p ada_tests
cd ada_tests

create() {
  name="$1"
  code="$2"
  out="$3"
  in="$4"

  printf '%s\n' "$code" >"$name.adb"
  printf '%s\n' "$out" >"$name.adb.txt"
  printf '%s' "$in" >"$name.adb.in"
}

# 1–20: basic arithmetic and simple ifs

create b1_arith_add1 'procedure Main is
  X : Integer := 1;
begin
  X := X + 2;
  Put_Line(str(X));
end Main;' '3'

create b1_arith_add2 'procedure Main is
  X : Integer := 10;
begin
  X := X + 15;
  Put_Line(str(X));
end Main;' '25'

create b1_arith_sub1 'procedure Main is
  X : Integer := 7;
begin
  X := X - 4;
  Put_Line(str(X));
end Main;' '3'

create b1_arith_mul1 'procedure Main is
  X : Integer := 6;
begin
  X := X * 7;
  Put_Line(str(X));
end Main;' '42'

create b1_arith_div1 'procedure Main is
  X : Integer := 18;
begin
  X := X / 3;
  Put_Line(str(X));
end Main;' '6'

create b1_arith_mix1 'procedure Main is
  X : Integer := 0;
begin
  X := 2 * 5 + 3;
  Put_Line(str(X));
end Main;' '13'

create b1_arith_mix2 'procedure Main is
  X : Integer := 0;
begin
  X := (2 + 5) * 3;
  Put_Line(str(X));
end Main;' '21'

create b1_pow1 'procedure Main is
  X : Integer := 3;
begin
  X := X ** 3;
  Put_Line(str(X));
end Main;' '27'

create b1_pow2 'procedure Main is
  X : Integer := 2;
begin
  X := X ** 5;
  Put_Line(str(X));
end Main;' '32'

create b1_chain_assign 'procedure Main is
  X : Integer := 1;
begin
  X := X + 1;
  X := X * 4;
  X := X - 2;
  Put_Line(str(X));
end Main;' '6'



create b1_if_else1 'procedure Main is
begin
  if 2 = 3 then
    Put_Line(str(1));
  else
    Put_Line(str(2));
  end if;
end Main;' '2'

create b1_if_else2 'procedure Main is
  X : Integer := 5;
begin
  if X > 3 then
    Put_Line(str(7));
  else
    Put_Line(str(8));
  end if;
end Main;' '7'


create b1_if_var2 'procedure Main is
  X : Integer := 3;
begin
  if X * 2 > 6 then
    Put_Line(str(1));
  else
    Put_Line(str(0));
  end if;
end Main;' '0'


create b1_if_bool2 'procedure Main is
  V : Boolean := False;
begin
  if V then
    Put_Line(str(1));
  else
    Put_Line(str(2));
  end if;
end Main;' '2'


