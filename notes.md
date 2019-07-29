# Miscellaneous

- file extension : .lm

# Features

- sous-typage 
- code compile-time
- effets algébriques + typage + continuations multishot
- pas d'objets
- état mutable

# Syntaxe

## divers

- majuscules obligatoires pour les constructeurs

## application de fonctions
f(a)
f(a, b)
f(?,b) = f : a |-> f(a,b)
f(a,?) = f : b |-> f(a,b)
f((a,b))

					(=> notion de nb d'arguments d'une fonction)

## définition

val f = y =>				(on autorise def f(y,z) {bla bla} et on autorise (=> 42))
  val x = 3;
  val z = 7;
  return x + y < z

					(on note (x => 2x) ou (x,y => x + y) pour les fonctions anonymes)
					(return optionnel)

## récursivité explicite (val rec, def rec)

## boucles
### for

for x in truc { machin(x) ; machin_bis(x)}

def rec loop(z) { match z with
  | () => ()
  | effect Iter(v) k => machin(v); loop(k())
};
loop(truc)

ou

val rec loop = (z => match z with
  | () => ()
  | effect Iter k v => machin(v); loop(k())
);
loop(truc)

range(n,m) = if n >= m then () else (perform Iter(n); range(n+1,m))

### while

while x {truc}

### if

if x {abc} else {def} 			(else optionnel si pas de 2e bloc)


## match

match truc {
| abc => truc_bis
| def => truc_ter
| exception Fsd(v) =>
| effect Iter(v) k => 			(la continuation est après)
}

## créer un effet

perform Nom_effet(arg)			(argument optionnel)


## listes, tableaux, tuples

### listes

[a, b, c]

[]

t::q

### tableaux

[| a, b, c |]

tab[i]
tab[i] = truc				(if (2==3) {truc} else {truc'})

### tuples

(a,b,c)					(parenthèses obligatoires)
fst(t)
snd(t)
					(pour les 2-tuples uniquement)

## types

a -> b -> c				(une fonction qui renvoie une fonction)
(a,b) -> c				(une fonction à plusieurs arguments)
(a*b*c) -> c				(une fonction qui prend un tuple en argument)

## types construits 

### types inductifs

type machin = truc | truc2 | Constructeur of truc3

### record

type machin = {champ1 : type1 ; ...}

machin.champ1
machin.champ2 = truc


