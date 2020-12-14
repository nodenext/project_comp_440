:- dynamic(known/3).

find_pokemon:- 
nl,
retractall(known(_,_,_)),         
pokemon(X),
nl,nl,
write('Pokemon is: '), write(X).

type(grass_type):-
 body(have_plant),
 need(sun).
type(poision_type):-
 body(purple).
type(fire_type):-
 body(have_fire),
 need(hot).
type(flying_type):-
 body(have_wing).
type(psychic_type):-
 need(mental_strength).
type(ice_type):-
 body(have_ice),
 need(cold).
type(normal_type):-
 all(ordinary).
type(fight_type):-
 attack(leg_hand).
type(ground_type):-
 need(ground).
type(rock_type):-
 body(have_rock),
 need(ground).
type(bug_type):-
 body(bug),
 need(tree).
type(ghost_type):-
 body(float),
 need(mental_strength).
type(steel_type):-
 body(metallic_luster).
type(water_type):-
 body(water_creatures),
 need(water).
type(electricity_type):-
 need(electric).
type(dragon_type):-
 body(scales).


category(seed):-
 type(grass_type),
 live(grassland_forest).
category(poison_needle):-
 type(poision_type),
 live(grassland_plains).
category(lizard):-
 type(fire_type),
 live(mountains_caves).
category(tiny_bird):-
 type(flying_type),
 live(grassland_forest).
category(psi):-
 type(psychic_type),
 live(less_wild).
category(fresh_snow):-
 type(ice_type),
 live(snow_mountains).
category(mouse):-
 type(normal_type),
 live(every_place).
category(pig_monkey):-
 type(fight_type),
 live(forest).
category(mole):-
 type(ground_type),
 live(underground_cave).
category(compass):-
 type(rock_type),
 live(cave).
category(worm):-
 type(bug_type),
 live(grassland_forest).
category(screech):-
 type(ghost_type),
 live(cemeteries_abandonedHouse).
category(gear):-
 type(steel_type),
 live(cave_mountain).
category(fish):-
 type(water_type),
 live(river_sea).
category(mouse):-
 type(electricity_type),
 live(forest).
category(dragon):-
 type(dragon_type),
 live(waterfall_pool).


pokemon(bulbasaur):-
 category(seed), 
 look_like(toad),
 eye(red),
 nose(short_blunt).
pokemon(nidoran):-
 category(poison_needle), 
 look_like(small_rodents),
 eye(samll_red),
 ear(big_cyan),
 limbs(two_toes).
pokemon(charmander):-
 category(lizard),
 look_like(carnivorous_dinosaur),
 eye(blue),
 limbs(short).
pokemon(pidgey):-
 category(tiny_bird), 
 look_like(little_bird),
 eye(black),
 head(three_tufts).
pokemon(alakazam):-
 category(psi), 
 look_like(human_type),
 finger(three),
 hold(spoon).
pokemon(vanillite):-
 category(fresh_snow),
 look_like(cone),
 eye(blue),
 face_side(quadrilateral_object).
pokemon(rattata):-
 category(mouse), 
 look_like(mouse),
 eye(red),
 color(purple).
pokemon(primeape):-
 category(pig_monkey), 
 look_like(primates),
 temperament(irritable),
 nose(like_pig),
 together(body_head).
pokemon(diglett):-
 category(mole),
 look_like(oval),
 nose(red),
 eye(black).
pokemon(nosepass):-
 category(compass), 
 look_like(stone),
 eye(black),
 arms(short),
 leg(short),
 color(purple).
pokemon(caterpie):-
 category(worm), 
 look_like(caterpillar),
 back(green),
 belly(white),
 tentacles(y_shape).
pokemon(diglett):-
 category(screech),
 color(dark_green),
 hair(wildfire),
 neck(red_beads),
 eye(red).
pokemon(klink):-
 category(gear), 
 look_like(gears),
 consists(two_gears).
pokemon(magikarp):-
 category(fish), 
 look_like(red_fish),
 fin(golden_crown).
pokemon(pikachu):-
 category(mouse),
 look_like(electric_mouse),
 fur(yellow),
 electric_airbag(red),
 tail(lightning),
 eye(black),
 we_call(god_pi).
pokemon(dratini):-
 category(dragon), 
 look_like(serpentine),
 whole_body(blue_scales),
 abdomen(white),
 eye(purple).

body(X):- ask(body,X).

need(X):- ask(need,X).

attack(X):- ask(attack,X).

all(X):- ask(all,X).

live(X):- ask(live,X).

look_like(X):-ask(look_like,X).

eye(X):- ask(eye,X).

nose(X):- ask(nose,X).

limbs(X):- ask(limbs,X).

ear(X):- ask(ear,X).

head(X):- ask(head,X).

face_side(X):- ask(face_side,X).

color(X):- ask(color,X).

temperament(X):- ask(temperament,X).

together(X):- ask(together,X).

arms(X):- ask(arms,X).
leg(X):- ask(leg,X).
back(X):- ask(back,X).
belly(X):- ask(belly,X).
hair(X):- ask(hair,X).
neck(X):- ask(neck,X).
consists(X):- ask(consists,X).
fin(X):-ask(fin,X).
fur(X):-ask(fur,X).
electric_airbag(X):- ask(electric_airbag,X).
tail(X):-ask(tail,X).
we_call(X):-ask(we_call,X).
whole_body(X):- ask(whole_body,X).
abdomen(X):- ask(abdomen,X).

multi(eye).
multi(live).
multi(look_like).
multi(body).
multi(color).
multi(need).

ask(X,Y):- \+multi(X),
           known(yes,X,Z),
           Y\==Z,!,fail.

ask(X,Y):- known(yes, X, Y),!.

ask(X,Y):- known(_, X, Y),
           !,fail. 

ask(X,Y):-write('Pokemon '),
          write(X),
          write(' is '),
          write(Y),
          write('?: (yes or no): '),
          read(Z),
          asserta(known(Z, X, Y)),
          Z==yes.


flat(normal, normal).
flat(normal, fight).
flat(normal, flying).
flat(normal, posion).
flat(normal, ground).
flat(normal, bug).
flat(normal, fire).
flat(normal, water).
flat(normal, electricity).
flat(normal, grass).
flat(normal, psychic).
flat(normal, ice).
flat(normal, dragon).
half(normal, rock).
half(normal, ghost).
half(normal, steel).

double(fight, normal).
double(fight, rock).
double(fight, steel).
double(fight, ice).
flat(fight,fight).
flat(fight,ground).
flat(fight,fire).
flat(fight,water).
flat(fight,grass).
flat(fight,electricity).
flat(fight,dragon).
half(fight,flying).
half(fight,posion).
half(fight,bug).
half(fight,ghost).
half(fight,psychic).

flat(flying, normal).
flat(flying, ground).
flat(flying, flying).
flat(flying, posion).
flat(flying, ghost).
flat(flying, fire).
flat(flying, water).
flat(flying, psychic).
flat(flying, ice).
flat(flying, dragon).
half(flying, rock).
half(flying, steel).
half(flying, electricity).
double(flying, fight).
double(flying, bug).
double(flying, grass).

flat(posion, normal).
flat(posion, fight).
flat(posion, flying).
flat(posion, bug).
flat(posion, fire).
flat(posion, water).
flat(posion, psychic).
flat(posion, ice).
flat(posion, dragon).
flat(posion, electricity).
half(posion, posion).
half(posion, ground).
half(posion, rock).
half(posion, ghost).
half(posion, steel).
double(posion, grass).

flat(ground, normal).
flat(ground, fight).
flat(ground, rock).
flat(ground, ghost).
flat(ground, water).
flat(ground, psychic).
flat(ground, ice).
flat(ground, dragon).
half(ground, flying).
half(ground, bug).
half(ground, grass).
double(ground, posion).
double(ground, fire).
double(ground, electricity).
double(ground, rock).
double(ground, steel).

flat(rock, normal).
flat(rock, posion).
flat(rock, rock).
flat(rock, ghost).
flat(rock, water).
flat(rock, grass).
flat(rock, electricity).
flat(rock, dragon).
half(rock, ground).
half(rock, fight).
half(rock, steel).
double(rock, bug).
double(rock, flying).
double(rock, fire).
double(rock, ice).

flat(bug, normal).
flat(bug, ground).
flat(bug, rock).
flat(bug, bug).
flat(bug, water).
flat(bug, electricity).
flat(bug, ice).
flat(bug, dragon).
half(bug, posion).
half(bug, flying).
half(bug, fight).
half(bug, fire).
half(bug, ghost).
half(bug, steel).
double(bug, grass).
double(bug, psychic).

flat(ghost, fight).
flat(ghost, flying).
flat(ghost, bug).
flat(ghost, fire).
flat(ghost, water).
flat(ghost, ice).
flat(ghost, dragon).
flat(ghost, electricity).
flat(ghost, posion).
flat(ghost, ground).
flat(ghost, rock).
flat(ghost, grass).
half(ghost, normal).
half(ghost, steel).
double(ghost, psychic).
double(ghost, ghost).

flat(steel, normal).
flat(steel, fight).
flat(steel, flying).
flat(steel, posion).
flat(steel, ground).
flat(steel, bug).
flat(steel, ghost).
flat(steel, grass).
flat(steel, psychic).
flat(steel, dragon).
half(steel, water).
half(steel, fire).
half(steel, electricity).
half(steel, steel).
double(steel, ice).
double(steel, rock).

flat(fire, normal).
flat(fire, fight).
flat(fire, flying).
flat(fire, posion).
flat(fire, ground).
flat(fire, ghost).
flat(fire, electricity).
flat(fire, psychic).
half(fire, fire).
half(fire, water).
half(fire, rock).
half(fire, dragon).
double(fire, grass).
double(fire, ice).
double(fire, bug).
double(fire, steel).


flat(water, normal).
flat(water, fight).
flat(water, flying).
flat(water, posion).
flat(water, bug).
flat(water, ghost).
flat(water, steel).
flat(water, electricity).
flat(water, psychic).
flat(water, ice).
half(water, water).
half(water, grass).
half(water, dragon).
double(water, ground).
double(water, fire).
double(water, rock).

flat(grass, normal).
flat(grass, fight).
flat(grass, ghost).
flat(grass, electricity).
flat(grass, psychic).
flat(grass, ice).
half(grass, bug).
half(grass, posion).
half(grass, flying).
half(grass, fire).
half(grass, grass).
half(grass, dragon).
half(grass, steel).
double(grass, ground).
double(grass, water).
double(grass, rock).


flat(electricity, normal).
flat(electricity, fight).
flat(electricity, posion).
flat(electricity, rock).
flat(electricity, bug).
flat(electricity, ghost).
flat(electricity, steel).
flat(electricity, fire).
flat(electricity, psychic).
flat(electricity, ice).
half(electricity, electricity).
half(electricity, grass).
half(electricity, dragon).
half(electricity, ground).
double(electricity, flying).
double(electricity, water).


flat(psychic, normal).
flat(psychic, flying).
flat(psychic, ground).
flat(psychic, rock).
flat(psychic, bug).
flat(psychic, ghost).
flat(psychic, fire).
flat(psychic, water).
flat(psychic, grass).
flat(psychic, electricity).
flat(psychic, ice).
flat(psychic, dragon).
half(psychic, psychic).
half(psychic, steel).
double(psychic, fight).
double(psychic, posion).


flat(ice, normal).
flat(ice, fight).
flat(ice, posion).
flat(ice, rock).
flat(ice, bug).
flat(ice, ghost).
flat(ice, electricity).
flat(ice, psychic).
half(ice, fire).
half(ice, water).
half(ice, ice).
half(ice, steel).
double(ice, flying).
double(ice, ground).
double(ice, grass).
double(ice, dragon).

flat(dragon, normal).
flat(dragon, fight).
flat(dragon, flying).
flat(dragon, posion).
flat(dragon, ground).
flat(dragon, rock).
flat(dragon, bug).
flat(dragon, ghost).
flat(dragon, fire).
flat(dragon, water).
flat(dragon, grass).
flat(dragon, electricity).
flat(dragon, psychic).
flat(dragon, ice).
half(dragon, steel).
double(dragon, dragon).



isbetter(X,Y):- half(X,Y).

is_flat(X,Y):-flat(X,Y).

is_double(X,Y):-double(X,Y).



checka(X):- 
member(X,[normal, grass,fight,flying,
	posion,ground,bug,rock,ghost,steel,psychic,
	electricity,ice,dragon,fire,water]).

showlist([]).

showlist([X|List]) :-
write(X),nl,
printlist(List).


fight_assume:-
write('pokemon attribute have'),nl,nl,

showlist([normal, grass,fight,flying,
	posion,ground,bug,rock,ghost,steel,psychic,
	electricity,ice,dragon,fire,water]),nl,
write('enter attack pokemon attribute: '),
read(X),checka(X),nl,
write('enter fight with pokemon attribute: '),
read(Y),checka(Y),nl,
((isbetter(X,Y)) -> write('Attributes are suppressed, attack is not recommended');
(is_flat(X,Y))-> write('Your optional, attributes are not suppressed, nor restrained');
write('Attribute suppression, can attack')).

%(is_doule(X,Y))->
