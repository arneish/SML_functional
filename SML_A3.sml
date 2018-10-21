(* SML (Standard Meta Language) Practice assignment for COL765, Sem-I 2018-19 
attempted by Arneish Prateek *)

(* SML 1.1 *)
exception ValueErr;
fun sum (m, n) = 
    if (m>n) then raise ValueErr else if (m=n) then n else m + sum(m+1, n);
(* SML 1.2 *)
fun bin_coeff (n, r) = 
    if (r>n) then raise ValueErr else if (r=1) then n else (n * bin_coeff(n-1, r-1)) div r;

(* SML 2.1 *)
datatype day_night = AM | PM ;
type time_triple = int * int * day_night;
fun before_time (time_1:time_triple, time_2:time_triple) =
    let 
        fun time_min (HH:int, MM:int, DD:day_night) =
            if (DD=PM) then HH*60+MM+720 else HH*60+MM;
    in 
        if time_min(time_1)<time_min(time_2) then print ("TRUE\n") else print ("FALSE\n")
    end
(* SML 2.2 *)
type time_record = {hour:int, min:int, f:day_night};
fun before_time_record (time_1: time_record, time_2:time_record) = 
    let 
        fun time_min {hour:int, min:int, f:day_night} = 
            if (f=PM) then hour*60 + min + 720 else hour*60+min;
    in 
        if time_min(time_1)<time_min(time_2) then print ("TRUE\n") else print ("FALSE\n")
    end

(* SML 3.1 *)
fun addpoly ([], polytwo) = 
    polytwo |
    addpoly (polyone, []) = polyone |
    addpoly ((coeff_one, exp_one)::polyone_tail, (coeff_two, exp_two)::polytwo_tail) =
    if (exp_one=exp_two) then [(coeff_one+coeff_two, exp_one)]@ addpoly(polyone_tail, polytwo_tail) else
    if (exp_one>exp_two) then [(coeff_one, exp_one)] @ addpoly (polyone_tail,(coeff_two, exp_two)::polytwo_tail) else
    [(coeff_two, exp_two)]@addpoly((coeff_one, exp_one)::polyone_tail, polytwo_tail);
(* Testing addpoly() *)
val poly1 = [(3,3), (2,2), (6,1), (5,0)];
val poly2 = [(4,4), (6,2), (~3,1)];
addpoly(poly1, poly2);
(* SML 3.2 *)
fun multpoly([], polytwo) = [(0,0)] | multpoly (polyone, []) = [(0,0)] | 
    multpoly((coeff_one, exp_one)::polyone_tail, (coeff_two, exp_two)::polytwo_tail) = 
    addpoly(addpoly([(coeff_one*coeff_two, exp_one+exp_two)], multpoly([(coeff_one, exp_one)], polytwo_tail)),multpoly(polyone_tail, (coeff_two, exp_two)::polytwo_tail));
(* Testing multpoly() *)
val poly3 = [(3,2), (~6,1), (~5,0)];
val poly4 = [(4,5), (~4,4), (3,1), (~10,0)];
val poly5 = [(1,2), (~2,1), (3,0)];
val poly6 = [(2,1), (~5,0)];
multpoly (poly3, poly4);

(* SML 4 *)
structure ComplexNumbers =
struct
    infix ++;
    infix **;
    infix inv;
    infix //;
    fun (a,b)++(c,d):real*real = 
        (a+c, b+d);
    fun (a,b)**(c,d):real*real = 
        (a*c-b*d, b*c+a*d);
    fun op inv (a,b):real*real =
        (a/(a*a+b*b), ~b/(a*a+b*b));
    fun (a,b)//(c,d):real*real = 
        (a,b)**(op inv(c,d));
end
(* Testing structure *)
val one = (3.0, 4.0);
val two = (3.0, 5.0);
structure P = ComplexNumbers;
P.++ (one, two);
P.** (one, two);
P.// (one, two);
P.inv (one);