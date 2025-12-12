procedure main is
    n : integer := 5;
    is_small : boolean;
    is_even  : boolean;

    p1 : string;
    p2 : string;
    p3 : string;
    p4 : string;
    p5 : string;
    p6 : string;
    msg_small_even    : string;
    msg_small_odd     : string;
    msg_big           : string;
begin
    is_small := n < 10;
    is_even  := (n / 2) * 2 = n;

    p1 := "n=";
    p2 := str(n);
    p3 := " is small";
    p4 := " and even";
    p5 := " and odd";
    p6 := " is big";

    msg_small_even := p1 & p2 & p3 & p4;
    msg_small_odd  := p1 & p2 & p3 & p5;
    msg_big        := p1 & p2 & p6;

    if is_small then
        if is_even then
            put_line(msg_small_even);
        else
            put_line(msg_small_odd);
        end if;
    else
        put_line(msg_big);
    end if;
end main;

