procedure Main is
  X : Integer := 1;
  I : Integer := 1;
begin
  while I <= 4 loop
    X := X * I;
    I := I + 1;
  end loop;
  Put_Line(str(X));
end Main;
