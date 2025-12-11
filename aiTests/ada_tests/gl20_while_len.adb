procedure Main is
  str : String := "          ";
  num : Integer := 0;
  i : Integer := 0;
begin
  Get_Line (str, num);
  while i < num loop
    Put_Line(str(i));
    i := i + 1;
  end loop;
end Main;
