package Pck is

    type A is tagged null record;

    type B is new A with null record;

    type I1 is interface;

    type I2 is interface and I1;

    type F1 is new A and I2 with null record;

    type F2 is new B and I1 with null record;    

end Pck;
