procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Get_Line (str, num);
  Put_Line (str);
  Put_Line(str(num));
end Main;
