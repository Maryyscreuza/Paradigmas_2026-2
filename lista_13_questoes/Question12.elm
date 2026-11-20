type Casa
    = Fixa Int
    | Preenchida Int
    | Vazia

A garantia desse tipo vem dos construtores exclusivos: uma casa só pode ser Fixa, Preenchida ou Vazia. Então, não existe uma representação válida que seja ao mesmo tempo fixa e vazia.