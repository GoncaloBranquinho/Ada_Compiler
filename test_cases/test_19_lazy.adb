procedure main is
    x  : integer := 3;
    y  : integer := 10;
    z  : integer := 0;
    b1 : boolean;
    msg1 : string;
    msg2 : string;
    msg3 : string;
    final : string;
begin
    b1 := (x < y) and (y > 0) or (z /= 0);

    if b1 then
        msg1 := str(x);
        msg2 := str(y);
        msg3 := str(z);
        final := msg1 & msg2 & msg3;
        put_line(final);
    else
        put_line("no");
    end if;
end main;

