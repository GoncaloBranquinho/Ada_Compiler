procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Get_Line (str, num);
  if num >= 4 then
    Put_Line(str(1));
  else
    Put_Line(str(0));
  end if;
end Main;
