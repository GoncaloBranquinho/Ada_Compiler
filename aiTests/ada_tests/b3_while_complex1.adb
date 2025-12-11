procedure Main is
  X : Integer := 5;
begin
  while (X > 0) and (X /= 2) loop
    Put_Line(str(X));
    X := X - 1;
  end loop;
end Main;
