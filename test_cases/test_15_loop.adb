procedure main is
    i   : integer := 1;
    sum : integer := 0;
begin
    while i <= 10 loop
        sum := sum + i;
        i := i + 1;
    end loop;
    put_line(str(sum));
end main;
