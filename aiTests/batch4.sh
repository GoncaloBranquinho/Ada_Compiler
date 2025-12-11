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

# 61–80: more arithmetic/conditions/loops

create b4_nested_else 'procedure Main is
  X : Integer := 5;
begin
    if X > 10 then
      Put_Line(str(1));
    else
      Put_Line(str(2));
    end if;
end Main;' '2'

create b4_nested_bool 'procedure Main is
  A : Boolean := True;
  B : Boolean := False;
begin
    if B then
      Put_Line(str(1));
    else
      Put_Line(str(2));
  end if;
end Main;' '2'

create b4_while_guard1 'procedure Main is
  X : Integer := 3;
  V : Boolean := True;
begin
  while V loop
    Put_Line(str(X));
    V := False;
  end loop;
end Main;' '3'

create b4_while_guard2 'procedure Main is
  X : Integer := 0;
  V : Boolean := False;
begin
  while V loop
    Put_Line(str(1));
  end loop;
end Main;' ''

create b4_while_cmp 'procedure Main is
  X : Integer := 1;
begin
  while X * 2 < 10 loop
    Put_Line(str(X));
    X := X + 1;
  end loop;
end Main;' '1234'

create b4_while_eq 'procedure Main is
  X : Integer := 0;
begin
  while X /= 3 loop
    Put_Line(str(X));
    X := X + 1;
  end loop;
end Main;' '012'

create b4_line_bool 'procedure Main is
  V : Boolean := True;
begin
  if V then
    Put_Line ("yes");
  else
    Put_Line ("no");
  end if;
end Main;' 'yes'

create b4_line_bool2 'procedure Main is
  V : Boolean := False;
begin
  if V then
    Put_Line ("yes");
  else
    Put_Line ("no");
  end if;
end Main;' 'no'

create b4_mixed_ops 'procedure Main is
  X : Integer := 2;
  Y : Integer := 3;
begin
  Put_Line(str(X * X));
  Put_Line(str(Y * Y));
end Main;' '49'

create b4_loop_lines 'procedure Main is
  I : Integer := 1;
begin
  while I <= 3 loop
    Put_Line ("z");
    I := I + 1;
  end loop;
end Main;' 'zzz'

create b4_loop_lines_num 'procedure Main is
  I : Integer := 1;
begin
  while I <= 2 loop
    Put_Line ("q");
    I := I + 1;
  end loop;
  Put_Line(str(9));
end Main;' 'qq9'

create b4_cond_expr 'procedure Main is
  X : Integer := 8;
begin
  if (X >= 5) and (X <= 10) then
    Put_Line(str(1));
  else
    Put_Line(str(0));
  end if;
end Main;' '1'

create b4_cond_expr2 'procedure Main is
  X : Integer := 4;
begin
  if (X < 0) or (X > 10) then
    Put_Line(str(1));
  else
    Put_Line(str(0));
  end if;
end Main;' '0'


create b4_cond_expr4 'procedure Main is
  X : Integer := 2;
begin
  if not (X > 1 and X < 3) then
    Put_Line(str(0));
  else
    Put_Line(str(1));
  end if;
end Main;' '1'

create b4_loop_dec2 'procedure Main is
  X : Integer := 8;
begin
  while X >= 4 loop
    Put_Line(str(X));
    X := X - 2;
  end loop;
end Main;' '864'

create b4_loop_inc2 'procedure Main is
  X : Integer := 0;
begin
  while X <= 6 loop
    Put_Line(str(X));
    X := X + 3;
  end loop;
end Main;' '036'
