package Pck is

    type A is tagged null record;

    type B is new A with null record;

    type I1 is interface;

    type F is new A and I1 with null record; 

end Pck;
