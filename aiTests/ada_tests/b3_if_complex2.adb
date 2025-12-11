procedure Main is
  X : Integer := 3;
begin
  if (X > 0) and not (X > 2 and X < 4) then
    Put_Line(str(1));
  else
    Put_Line(str(0));
  end if;
end Main;
