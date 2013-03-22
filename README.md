[![Build Status](https://travis-ci.org/kgadek/Amber.png?branch=master) "Master branch"](http://github.com/kgadek/Amber)

[![Build Status](https://travis-ci.org/kgadek/Amber.png?branch=develop) "Develop branch"](http://github.com/kgadek/Amber)

                                                          bbbbbbbb                                                      
                   AAA                                    b::::::b            EEEEEEEEEEEEEEEEEEEEEE                    
                  A:::A                                   b::::::b            E::::::::::::::::::::E                    
                 A:::::A                                  b::::::b            E::::::::::::::::::::E                    
                A:::::::A                                  b:::::b            EE::::::EEEEEEEEE::::E                    
               A:::::::::A            mmmmmmm    mmmmmmm   b:::::bbbbbbbbb      E:::::E       EEEEEErrrrr   rrrrrrrrr   
              A:::::A:::::A         mm:::::::m  m:::::::mm b::::::::::::::bb    E:::::E             r::::rrr:::::::::r  
             A:::::A A:::::A       m::::::::::mm::::::::::mb::::::::::::::::b   E::::::EEEEEEEEEE   r:::::::::::::::::r 
            A:::::A   A:::::A      m::::::::::::::::::::::mb:::::bbbbb:::::::b  E:::::::::::::::E   rr::::::rrrrr::::::r
           A:::::A     A:::::A     m:::::mmm::::::mmm:::::mb:::::b    b::::::b  E:::::::::::::::E    r:::::r     r:::::r
          A:::::AAAAAAAAA:::::A    m::::m   m::::m   m::::mb:::::b     b:::::b  E::::::EEEEEEEEEE    r:::::r     rrrrrrr
         A:::::::::::::::::::::A   m::::m   m::::m   m::::mb:::::b     b:::::b  E:::::E              r:::::r            
        A:::::AAAAAAAAAAAAA:::::A  m::::m   m::::m   m::::mb:::::b     b:::::b  E:::::E       EEEEEE r:::::r            
       A:::::A             A:::::A m::::m   m::::m   m::::mb:::::bbbbbb::::::bEE::::::EEEEEEEE:::::E r:::::r            
      A:::::A               A:::::Am::::m   m::::m   m::::mb::::::::::::::::b E::::::::::::::::::::E r:::::r            
     A:::::A                 A:::::A::::m   m::::m   m::::mb:::::::::::::::b  E::::::::::::::::::::E r:::::r            
    AAAAAAA                   AAAAAAAmmmm   mmmmmm   mmmmmmbbbbbbbbbbbbbbbb   EEEEEEEEEEEEEEEEEEEEEE rrrrrrr            
                                                                                                                      

# Projekt "Amber"

Projekt inżynierski Konrada Gądka i Michała Konarskiego. Składa się z

  - mediatora (Erlang)
  - sterowników urządzeń (C/C++)
  - aplikacji klienckich (Erlang, Java)

Projekt jest w stadium rozwojowym. Wszystkie flagi debugowania są włączone.

## Dwa słowa o testach.

Wow. Istnieją.


## Pierwsza kompilacja

1. `make all` # ściąga zależności, kompiluje projekt i sterowniki
1. `make test` # testy

## Następne kompilacje

  - `make` # kompiluje zmiany wprowadzone do mediatora (`amber`) i klienta (`amber_client`)
  - `make drivers` # kompiluje wszystkie sterowniki
  - `make roboclaw_drivers` # kompiluje konkretny sterownik

## Uruchamianie

W skrócie: serwer zwykle będzie działać wiecznie (w wypadku awarii sam się zrestartuje),
klient jest /jednorazowy/.

### Uruchomienie serwera (tryb: nieśmiertelny)

1. Otwórz skrypt start_amber i popraw zmienną PTH -- ustaw ją na bieżący katalog, dodaj "/" na końcu
2. Wywołaj polecenie `./start_amber`

### Zatrzymanie serwera (tryb: nieśmiertelny)

Niezbyt intuicyjne: `killall heart`

### Uruchomienie serwera z interaktywną konsolą

Z pliku `start_amber` wyciągnąć polecenie uruchomieniowe i usunąć `-detached -heart` -- te
dwie opcje oznaczają (odpowiednio) uruchomienie bez konsoli i uruchomienie procesu-nadzorcy,
sprawdzającego, czy Erlangowa VM nadal żyje.

A, jeszcze jedno -- zamknięcie aplikacji `amber` spowoduje zamknięcie całej
maszyny wirtualnej. Odpowiada za to opcja `permanent`.
Patrz: plik `amber.erl`, linia 8: `start() -> application:start(amber, permanent).`

### Uruchomienie klienta z interaktywną konsolą

Poniższe polecenie otwiera shell z załadowanym klientem:

    ./start_client

## Git workflow

Warto przeglądnąć artykuł [A successful Git branching model](http://nvie.com/posts/a-successful-git-branching-model/).
Poniższy obrazek prezentuje koncept. Może wydawać się to skomplikowane, ale takim nie jest.
Opcja `--no-ff` została domyślnie ustawiona na branche `master` oraz `develop`.

![Workflow](http://nvie.com/img/2009/12/Screen-shot-2009-12-24-at-11.32.03.png)

