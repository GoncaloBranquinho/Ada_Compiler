procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Get_Line (str, num);
  if num > 3 then
    Put_Line (str);
  else
    Put_Line(str(0));
  end if;
end Main;
