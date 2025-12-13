procedure main is
    x : String := "ola";
    y : String := "adeus";
    z : String;
    k : String;
begin
    z := x & y;
    k := y & x;
    put_line(z);
    put_line(k);
end main;
