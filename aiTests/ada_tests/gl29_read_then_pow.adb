procedure Main is
  str : String := "          ";
  num : Integer := 0;
  x : Integer := 2;
begin
  Get_Line (str, num);
  x := x ** num;
  Put_Line(str(x));
end Main;
