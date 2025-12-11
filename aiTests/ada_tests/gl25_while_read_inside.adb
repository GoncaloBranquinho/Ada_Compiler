procedure Main is
  str : String := "          ";
  num : Integer := 0;
  i : Integer := 0;
begin
  while i < 3 loop
    Get_Line (str, num);
    Put_Line(str(num));
    i := i + 1;
  end loop;
end Main;
