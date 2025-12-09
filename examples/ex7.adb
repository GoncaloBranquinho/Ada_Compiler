procedure Main is
  x : Float := 3.2;
  y : String := "o";
  z : Integer;
  p : String;
  k : String := "";
begin
  get_line(p, z);
  while (z > 0) loop
    z := z - 1;
    k := p & y;
  end loop;
  put_line(k);
end Main;
