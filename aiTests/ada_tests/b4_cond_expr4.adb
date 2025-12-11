procedure Main is
  X : Integer := 2;
begin
  if not (X > 1 and X < 3) then
    Put_Line(str(0));
  else
    Put_Line(str(1));
  end if;
end Main;
