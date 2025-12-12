procedure main is
    a : integer := 1;
    b : integer := 2;
    c : integer := 3;
    d : integer := 4;
    e : integer := 5;
    f : integer := 6;
    g : integer := 7;
    h : integer := 8;
    r : integer;
begin
    r := (a + b) * (c + d) + (e * f) - (g ** 2) + h;
    put_line(str(r));
end main;
