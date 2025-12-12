procedure main is
    a : boolean := true;
    s1 : string;
    s2 : string;
    s3 : string;
    s4 : string;
    res : string;
begin
    s1 := "Hello";
    s2 := ", ";
    s3 := "World";
    s4 := "!";
    
    if a then
        res := s1 & s2 & s3 & s4;
        put_line(res);
    else
        put_line("skip");
    end if;
end main;

