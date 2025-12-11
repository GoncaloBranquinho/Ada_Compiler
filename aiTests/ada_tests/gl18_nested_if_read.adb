procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Get_Line (str, num);
    if num < 5 then
      Put_Line (str);
    else
      Put_Line(str(num));
  end if;
end Main;
