using System;
using P;

class Program {
    static void Main(string[] args) {
        Rec r = PUnit.Proxy_Rec_Proxy_Allocate ();
        PUnit.Set_Proxy_A (r, 98);
        PUnit.Set_Proxy_B (r, 99);     
        PUnit.Some_P (r);   

        Rec r2 = PUnit.Some_F ();
        Console.WriteLine (PUnit.Get_Proxy_A (r2) + ", " + PUnit.Get_Proxy_B (r2));
    }
}
