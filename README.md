
`*`*La version en français suit la version en anglais*`*`  
`*`*French version follows*`*`

# Functional language interpreter

## Description

Implementation of part of a functional programming language interpreter (similar to Lisp) named **Slip**.  
  
<ins>Implements</ins> :
* Lexical and syntactic analysis
* Compilation
* Expression evaluation by interpretation
* Type checking
* Syntactic sugar.

## How to Install

* Clone or download the project from this GitHub repository.
* **Haskell :** GHC version 9.2.8

## How to run

1. Open the Terminal.
2. Locate the downloaded project.
3. Type `ghci` to starts GHC’s interactive environment. 
4. Type `run "examples.slip"` to execute the examples file.
5. You'll see the example functions' return values in your terminal.

The project already includes a few examples of possible functions :

* `exemples.slip`.

If you want to create your own functions, make sure you respect the syntax of the language. *(You'll find Slip's syntax in the next section.)*

## Slip documentation

````txt
- SYNTAX :

    τ ::= Int                 Integer type
        | Bool                Bool type 
        | (τ1 ... τn → τ )    Function type 
        | (Ref τ )            Ref-cell type 

    e ::= n                                 Decimal signed integer 
        | x                                 Variable 
        | (: e τ )                          Type anotation
        | (begin e1 ... e2)                 Instruction sequence
        | (λ (x1 ... xi) e1 ... en)         i arguments function
        | (e0 e1 ... en)                    Curried function call
        | (ref ! e)                         Ref-cell construction
        | (get ! e)                         Find the value of the ref-cell e 
        | (set ! e1 e2)                     Change the value of the ref-cell e1 
        | + | − | ∗ | /                     Pre-defined arithmetic operations
        | < | > | = | <= | >=               Boolean operations on integers
        | (if e1 e2 e3)                     if e1 then e2 else e3 
        | (let x ex e1 ... en)              Simple local declaration 
        | (letrec (d1 ... di) e1 ... en)    Recursive local declarations

    d ::= (x e) 
        | ((x (x1 τ1) ... (xn τn)) τ e1 ... en)



- SYNTACTIC SUGAR :

           (e1 e2 ... en) ≃ ((e1 e2) ... en)
        (begin e1 ... en) ≃ (let _ e1 (let _ e2 (... en)..)) 
         (λ xs e1 ... en) ≃ (λ xs (begin e1 ... en))
        (λ (x1 ... xn) e) ≃ (λ x1 ...(λ xn e)..)
     (let x ex e1 ... en) ≃ (let x ex (begin e1 ... en))
    (letrec ds e1 ... en) ≃ (letrec ds (begin e1 ... en))
````

## Credits

This project is a practical work for the course IFT2035 (Programming language concepts). It was made in collaboration with [Étienne Mitchell-Bouchard](https://github.com/DarkZant).

<br><br>
___

<br>

# Interpréteur de language fonctionnel

## Description

Implémentation d‘une partie d’un interpréteur d’un langage de programmation fonctionnel (similaire à Lisp) nommé **Slip**.  
  
<ins>Implémente</ins> :
* Analyse lexicale et syntaxique
* Compilation
* Évaluation des expressions par interprétation
* Vérification de types
* Sucre syntaxique.

## Comment installer

* Cloner ou télécharger le projet depuis ce dépôt GitHub.
* **Haskell :** GHC version 9.2.8

## Comment exécuter

1. Ouvrez la console de votre ordinateur.
2. Localisez le projet téléchargé.
3. Écrivez `ghci` pour démarrer l'environnement intéractif GHC.
4. Écrivez `run "examples.slip"` pour exécuter le fichier d'exemples.
5. Vous verrez les valeurs de retours des fonctions d'exemple s'afficher dans votre console.

Le projet contient déjà quelques exemples de fonctions :

* `exemples.slip`.

Si vous voulez créer vos propres fonctions, assurez-vous de respecter la syntaxe du langage. *(Vous trouverez la syntaxe de Slip dans la section suivante.)*

## Slip documentation

````txt
- SYNTAXE :

    τ ::= Int                 Type des nombres entiers 
        | Bool                Type des booléens 
        | (τ1 ... τn → τ )    Type d’une fonction 
        | (Ref τ )            Type d’une ref-cell 

    e ::= n                                 Un entier signé en décimal 
        | x                                 Une variable 
        | (: e τ )                          Annotation de type 
        | (begin e1 ... e2)                 Séquence d’instructions 
        | (λ (x1 ... xi) e1 ... en)         Une fonction avec i arguments 
        | (e0 e1 ... en)                    Un appel de fonction (curried) 
        | (ref ! e)                         Construction d’une ref-cell 
        | (get ! e)                         Chercher la valeur de la ref-cell e 
        | (set ! e1 e2)                     Changer la valeur de la ref-cell e1 
        | + | − | ∗ | /                     Opérations arithmétiques prédéfinies 
        | < | > | = | <= | >=               Opérations booléennes sur les entiers 
        | (if e1 e2 e3)                     Si e1 alors e2 sinon e3 
        | (let x ex e1 ... en)              Déclaration locale simple 
        | (letrec (d1 ... di) e1 ... en)    Déclarations locales récursives 

    d ::= (x e) 
        | ((x (x1 τ1) ... (xn τn)) τ e1 ... en)



- SUCRE SYNTAXIQUE :

            (e1 e2 ... en) ≃ ((e1 e2) ... en)
         (begin e1 ... en) ≃ (let _ e1 (let _ e2 (... en)..)) 
          (λ xs e1 ... en) ≃ (λ xs (begin e1 ... en))
         (λ (x1 ... xn) e) ≃ (λ x1 ...(λ xn e)..)
      (let x ex e1 ... en) ≃ (let x ex (begin e1 ... en))
      (letrec ds e1 ... en) ≃ (letrec ds (begin e1 ... en))
````

## Crédits

Ce projet est un travail pratique du cours IFT2035 (Concepts des langages de programmation). Il a été effectué en groupe avec [Étienne Mitchell-Bouchard](https://github.com/DarkZant).

<br>

```txt
©  /\\/\//\//\
  \|/|\|/|\|/|/
 /|/|\|/|\|\’ .\
 ‘`|_|————|_|———o
```
