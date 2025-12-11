procedure Main is
  I : Integer := 1;
  S : Integer := 0;
begin
  while I <= 5 loop
    S := S + I * I;
    I := I + 1;
  end loop;
  Put_Line(str(S));
end Main;
