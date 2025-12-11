procedure Main is
  X : Integer := 5;
begin
  if (X > 0) and (X < 10) and (X /= 3) then
    Put_Line(str(1));
  else
    Put_Line(str(0));
  end if;
end Main;
