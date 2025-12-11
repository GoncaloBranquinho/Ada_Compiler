procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Get_Line (str, num);
  Put_Line(str(num * 2));
end Main;
