procedure Main is
  X : Integer := 5;
begin
  while 10 > X loop
    Put_Line(str(X));
    X := X + 2;
  end loop;
end Main;
