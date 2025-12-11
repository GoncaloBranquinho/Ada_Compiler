procedure Main is
  X : Integer := 3;
  V : Boolean := True;
begin
  while (X > 0) and V loop
    Put_Line(str(X));
    X := X - 1;
  end loop;
end Main;
