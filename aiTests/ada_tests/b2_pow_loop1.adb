procedure Main is
  X : Integer := 1;
  I : Integer := 0;
begin
  while I < 4 loop
    X := X * 2;
    I := I + 1;
  end loop;
  Put_Line(str(X));
end Main;
