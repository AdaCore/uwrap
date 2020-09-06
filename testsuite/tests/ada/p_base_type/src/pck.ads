package Pck is

    type A is tagged null record;

    type B is new A with null record;

    type C is new B with null record;

    type D is new C with null record;

    type D2 is new C with null record;

    type E is new D with null record;

    type I1 is interface;

    type I2 is interface and I1;

    type I3 is interface and I2;

    type I4 is interface and I3;

    type F is new A and I4 with null record; 

end Pck;
