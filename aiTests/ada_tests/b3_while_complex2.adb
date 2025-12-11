procedure Main is
  X : Integer := 0;
begin
  while (X < 5) or (X = 10) loop
    Put_Line(str(X));
    X := X + 2;
  end loop;
end Main;
