:-dynamic  medication/5.
:-dynamic needrestock/1.
:-dynamic  expiredfound/1.


add_medication(Name, Expdate, Quantity, Unit, Type):- (medication(Name,Expdate,Oldquantity,Unit,Type) -> Newquantity is Oldquantity+Quantity,
                                                       retract(medication(Name,Expdate,Oldquantity,Unit,Type)),
                                                       assert(medication(Name,Expdate,Newquantity,Unit,Type)),
                                                       write('medication added to existing batch successfuly'),nl;
                                                      ( needrestock(Name)->retract(needrestock(Name));true),
                                                      assert(medication(Name,Expdate,Quantity,Unit,Type)),                                                                                                  write('a new medication batch added successfuly'),nl).

remove_medication(Name,Expdate, Unit):-(medication(Name,Expdate,_,Unit,_)->retract(medication(Name,Expdate,_,Unit,_)),
                                        write('medication removed successfuly');write('no such medication found'),nl).

remove_medication_quantity(Name,Expdate,Removingquantity,Unit):-(medication(Name,Expdate,Oldquantity,Unit,Type)->
                                                                Newquantity is Oldquantity-Removingquantity,
                                                                retract(medication(Name,Expdate,Oldquantity,Unit,Type)),
                                                                (Newquantity>0 ->
                                                                assert(medication(Name,Expdate,Newquantity,Unit,Type)),
                                                                write('Quantity deducted succesfuly'),nl;
                                                                assert(needrestock(Name)),
                                                                write('Removing quantity exeeded the existing quantity, therefore removed the whole
                                                                medication from the inventory'),nl);
                                                                write('no such medication found'),nl).



remove_all_medication_of_name_rec(Name):-medication(Name,_,_,_,_),(retract(medication(Name,_,_,_,_)),fail).

remove_all_medication_of_name(Name):-(medication(Name,_,_,_,_)->(remove_all_medication_of_name_rec(Name)->true;write('inventory items deleted'));write('No such medication found')).




remove_all_medication_of_name_rec(Name, Unit):-medication(Name,_,_,Unit,_),retract(medication(Name,_,_,Unit,_)),fail.

remove_all_medication_of_name(Name, Unit):-(medication(Name,_,_,Unit,_)->(remove_all_medication_of_name_rec(Name, Unit)->true;write('inventory items deleted'));write('No such medication found')).






check_availability(Name, Neededquantity, Unit):-bagof(Quantity,medication(Name,_,Quantity,Unit,_),L),
                                                sum_list(L, Totalquantity),
                                                (   Totalquantity>=Neededquantity->
                                                format('Medication ~w is available in the needed amount ~d ~n', [Name,Neededquantity]);
                                                  format('Medication ~w is NOT available in the needed amount.~d ~n. Only amount ~d is available ~n', [Name,Neededquantity, Totalquantity])).



check_quantity(Name):-findall(Quantity,medication(Name,_,Quantity,_,_),L),
                                                sum_list(L, Totalquantity),
                                                format('~d items of ~w available',[Totalquantity, Name]).


check_quantity_with_unit(Name, Unit):-findall(Quantity,medication(Name,_,Quantity,Unit,_),L),
                                                sum_list(L, Totalquantity),
                                                format('~d items of ~w ~w available',[Totalquantity, Name, Unit]).




traversal_and_check([],_,_):-!,fail.
traversal_and_check([H|T],Y,X):-(H>=Y->X is H;traversal_and_check(T,Y,X)).





medication('Paracetamol', '2022-01-01', 100,'pills','medicine').
medication('Paracetamol', '2024-06-01', 150,'pills','medicine').
medication('Ibuprofen', '2024-06-01', 50,'pills','medicine').
medication('Aspirin', '2025-12-01', 75,'pills','medicine').
medication('Surgical Spirit', '2024-12-01', 15,'100ml bottles','first aid').
medication('Bandages', '2026-12-01', 25,'rolls','first aid').
medication('Plasters', '2026-12-01', 15,'rolls','first aid').
medication('Naproxen', '2022-12-01', 15,'pills','medicine').
needrestock('periton').
needrestock('Amoxicillin').



list_medications:- write('medications in inventory'),nl,
                    medication(Name,Expdate,Quantity,Unit,Type),
                    format('Name : ~w, Expiery date : ~w, Quantity: ~d, Measurement unit:~w, Type: ~w',[Name,Expdate,Quantity,Unit,Type]),
                    nl,fail.
list_medications.



check_expiery_date(Name):-(write('expiery dates of the medicine batches'),nl,
                          medication(Name,Expdate,Quantity,Unit,_),
                          format('Expiery date: ~w     Batch quantity: ~d ~w', [Expdate,Quantity,Unit]),
                          nl,fail->true;true).



:- use_module(library(date)).
:- use_module(library(time)).

get_current_date(Date) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(date, DateTime, Date).

is_expired(Name) :-
    get_current_date(CurrentDate),
    medication(Name, Expdate, Quantity, _, _),
    parse_date(Expdate, ExpdateDateTime),
    expired2(ExpdateDateTime, CurrentDate),
    format('A ~d item batch of ~w was expired on ~w~n', [Quantity, Name, Expdate]),
    fail.

is_expired(_):-(expiredfound(1)->retract(expiredfound(1));!,fail).



expired2(date(Year1, Month1, Day1), date(Year2, Month2, Day2)) :-
    date(Year1, Month1, Day1) @< date(Year2, Month2, Day2),
    (   expiredfound(1)->true;assert(expiredfound(1))).



is_expired(Name,Expdate,Unit) :-
    get_current_date(CurrentDate),!,
    medication(Name, Expdate, _, Unit, _),
    parse_date(Expdate, ExpdateDateTime),
    !,
    expired(ExpdateDateTime, CurrentDate).

is_expired(_,_,_).


parse_date(Expdate, date(Year, Month, Day)) :-
    atomic_list_concat([YearAtom, MonthAtom, DayAtom], '-', Expdate),
    atom_number(YearAtom, Year),
    atom_number(MonthAtom, Month),
    atom_number(DayAtom, Day).

expired(date(Year1, Month1, Day1), date(Year2, Month2, Day2)) :-
    date(Year1, Month1, Day1) @< date(Year2, Month2, Day2).



remove_expired_items:- medication(Name,Expdate,Quantity,Unit,Type),
                       is_expired(Name,Expdate,Unit),
                       retract(medication(Name,Expdate,Quantity,Unit,Type)),
                       format('deleted ~d item batch of ~w, which was expired on ~w',[Quantity,Name,Expdate]),nl,
                       fail.

remove_expired_items.



display_expired_items:-medication(Name,Expdate,Quantity,Unit,Type),
                       is_expired(Name,Expdate,Unit),
                       format('item: ~w ~w, quantity: ~d, Type: ~w, EXPIRED ON: ~w',[Name,Unit,Quantity,Type,Expdate]),nl,
                       fail.

display_expired_items.





get_items_need_for_restock:-needrestock(Name),format('~w needs restock',[Name]),nl,fail.
get_items_need_for_restock.



check_medicine(Name,Unit):-(medication(Name,_,_,Unit,_)->true;format('you dont have any ~w ~w in your inventory',[Name, Unit]),!,fail).

haveMedicineFor(cold):-check_medicine('Periton','pills'),
                        check_medicine('Ventolin','pills'),
                        check_medicine('Paracetamol','pills').

haveMedicineFor('sore throat'):-check_medicine('Naproxen','pills'),
                        check_medicine('Ibuprofen','pills').



listMedicineFor(cold):-write('Periton, Ventolin, Paracetamol').
listMedicinefor('sore throat'):-write('Naproxen, Ibuprofen').


