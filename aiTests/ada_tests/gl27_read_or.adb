procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Get_Line (str, num);
  if (num < 2) or (num > 5) then
    Put_Line(str(1));
  else
    Put_Line (str);
  end if;
end Main;
