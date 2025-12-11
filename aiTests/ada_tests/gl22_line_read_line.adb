procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Put_Line ("before");
  Get_Line (str, num);
  Put_Line ("after");
  Put_Line(str(num));
end Main;
