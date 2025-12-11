procedure Main is
  x : Integer := 1;
begin
  if x > 0 then
    x := 0;
  else
    x := 1;
  end if;
  Put_Line(str(x));
end Main;
