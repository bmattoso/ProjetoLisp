;;;;;;;;;;;;;;;;;;;;;;;;;
; Funções auxiliares

(defun and (E1 E2)
	(cond 
		((equal E1 't) (cond 
							((equal E2 't))
							('t 'nil)
						)
		)
		('t 'nil)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun or (E1 E2)
	(cond
		((equal E1 't) 't)
		((equal E2 't) 't)
		('t 'nil)	
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun not (E1)
	(cond 
		(E1 'nil) ; Se entrar aqui então a função deu 't 
		('t 't)  ; Caso contrário devolver true
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maiorNeg (X1 X2) ; X1 é maior que X2?
	   (cond 
	   		((and (equal X1 0) (equal X2 0)) 'nil)
	   		((equal X1 0) 't)
	   		((equal X2 0) 'nil)
	   		('t (maiorNeg (1+ X1) (1+ X2)))
	   )
)

(defun maiorPos (X1 X2) ; X1 é maior que X2?
		(cond 
			((and (equal X1 0) (equal X2 0)) 'nil)
	   		((equal X1 0) 'nil)
	   		((equal X2 0) 't)
	   		('t (maiorPos (1- X1) (1- X2)))
		)
)

(defun maiorQue (X1 X2) 
	(cond 
		((and (ehNeg X1) (ehNeg X2)) (maiorNeg X1 X2)) ; Se ambos forem negativos somar para ver o maior
		((and (ehNeg X1) (not (ehNeg X2))) 'nil)
		((and (not (ehNeg X1)) (ehNeg X2))  't)
		('t (maiorPos X1 X2)) ; Ambos são p
		ositivos
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ehNeg (X)
 	(cond
 		((equal (1+ X) 'nil) 'nil) 
 		((atom X) (neg X X))
 		('t 'nil)
	)
)

(defun neg (num1 num2)
	(cond 
		((equal num1 0) 'nil)
		((equal num2 0) 't)
		('t (neg (1- num1) (1+ num2)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inverterAux (X1 aux)
	(cond 
		((equal aux 0) X1)
		((ehNeg aux) (inverterAux (1+ X1) (1+ aux)))
		('t 		(inverterAux (1- X1) (1- aux)))
	)
)

(defun inverter (X)
	(inverterAux X (mult X '2))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ehValidoAux (X)
	(cond 
		((or (equal X '+) (equal X '-)) 't)
		((or (equal X '*) (equal X '/)) 't)
		((or (equal X '^) (equal X '%)) 't)
		((or (equal X 'INFINITO) (equal X '-INFINITO)) 't)
		('t (or (not (equal 'nil (1+ X))) (equal 'nil X)))
	)
)

(defun ehValido (L)
	(cond 
		((not (atom (car L))) (and (ehValido (cdr L)) (ehValido (car L))))
		((equal (cdr L) 'nil) (ehValidoAux (car L))) ; Se o cdr da lista der nil, quer dizer que esta lista acabou 
		; Se passou lista vazia
		((equal (car L) 'nil) 't)
		((atom (car L)) (and (ehValidoAux (car L)) (ehValido (cdr L))))
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;
; Definição de funções básicas
; Soma,Subtração,Multiplicação,Divisão,Potenciação


; 1.	∞ + n = ∞
; 2.	-∞ + n = -∞
; 3.	∞ - n = ∞
; 4.	-∞ - n = -∞
; 5.	∞ + ∞ = ∞
; 6.	∞ - ∞ = NaN

(defun somarSubtrair (X1 X2)
	(cond 
		((and (or  (equal X1 'INFINITO) (equal X1 '-INFINITO)) (not (or  (equal X2 'INFINITO) (equal X2 '-INFINITO)))) X1)
		((and (or  (equal X2 'INFINITO) (equal X2 '-INFINITO)) (not (or  (equal X1 'INFINITO) (equal X1 '-INFINITO)))) X2)
		((and (equal X1 'INFINITO) (equal X2 'INFINITO)) 'INFINITO)
		((and (or  (equal X1 'INFINITO) (equal X1 '-INFINITO)) (or  (equal X2 'INFINITO) (equal X2 '-INFINITO))) 'NaN)
		((equal X1 0) X2); Se o primeiro número for 0 retornar o X2
		((equal X2 0) X1); Se o segundo número for 0 retornar o X1
		((ehNeg X1)   (somarSubtrair (1+ X1) (1- X2)))
		('t           (somarSubtrair (1- X1) (1+ X2)))
	)
)


; 7.	∞ . n = ∞, se n>0
; 8.	∞ . -n = -∞, se n<0
; 9.	∞ . 0 = NaN
; 10.	-∞ . n = -∞, se n>0
; 11.	-∞ . -n = ∞, se n<0
; 12.	-∞ . 0 = NaN
; 13.	∞ . ∞ = ∞
; 14.	∞ . -∞ = -∞
; 15.	-∞ . -∞ = ∞


(defun multNeg (X1 X2)
	(cond 
		((or  (equal X1 0) (equal X2 0)) 0)
		('t (somarSubtrair (multNeg (1+ X1) X2) X2))
	)
)

(defun mult (X1 X2)
	(cond 
		;; VERIFICAÇÕES DE INFINITO
		((and (equal X1 'INFINITO) (equal X2 '-INFINITO)) '-INFINITO)
		((and (equal X1 '-INFINITO) (equal X2 'INFINITO)) '-INFINITO)
		((and (equal X1 'INFINITO) (equal X2 'INFINITO)) 'INFINITO)
		((and (equal X1 '-INFINITO) (equal X2 '-INFINITO)) 'INFINITO)
		((and (or  (equal X1 'INFINITO) (equal X1 '-INFINITO)) (equal X2 0)) 'NAN)
		((and (or  (equal X2 'INFINITO) (equal X2 '-INFINITO)) (equal X1 0)) 'NAN) 
		((equal X1 'INFINITO) (cond 
									((ehNeg X2) '-INFINITO)
									('t 'INFINITO)
							   )
		) 
		
		((equal X1 '-INFINITO) (cond 
									((ehNeg X2) 'INFINITO)
									('t '-INFINITO)
							   )
		)
		((equal X2 'INFINITO) (cond 
									((ehNeg X1) '-INFINITO)
									('t 'INFINITO)
							   )
		) 
		
		((equal X2 '-INFINITO) (cond 
									((ehNeg X1) 'INFINITO)
									('t '-INFINITO)
							   )
		)

		((or  (equal X1 0) (equal X2 0)) 0)
		((and (ehNeg X1) (ehNeg X2)) (inverter (multNeg X1 X2))) ; Se os dois forem negativos
		((and (not (ehNeg X1)) (ehNeg X2)) (somarSubtrair (mult (1- X1) X2) X2))
		((and (ehNeg X1) (not (ehNeg X2))) (somarSubtrair (mult (1- X2) X1) X1))
		('t   (somarSubtrair (mult (1- X1) X2) X2)) ; Se ambos positivos
	)
)


; 16.	n / 0 = ∞, se n≠0
; 22.	0 / 0 = NaN

; 18.	∞ / n = ∞, n≥0
; 19.	∞ / n = -∞, n<0
; 20.	-∞ / n = -∞, n≥0
; 21.	-∞ / n = ∞, n<0

; 17.	n / ∞ = 0, inclusive para n =0
; 23.	∞ / ∞ = NaN
; 24.	∞ / -∞ = NaN
; 25.	-∞ / ∞ = NaN
; 26.	-∞ / -∞ = NaN


(defun divAux (X1 X2) ; Divide X1 por X2
	(cond 
		((equal X1 0) 0)
		((maiorQue X2 X1) 0)
		('t (1+ (divAux (somarSubtrair X1 (mult X2 '-1)) X2)))
	)
)


(defun div (X1 X2)
	(cond 
		((equal X2 0) (cond
							((equal X1 0) 'NaN)
							('t 'INFINITO)
					  )
		)
		;;;; Se o X2 for algum infinito 
		;;; Verificar as contas
		((or (equal X2 'INFINITO) (equal X2 '-INFINITO))
				 (cond
					((or (equal X1 'INFINITO) (equal X1 '-INFINITO)) 'NaN)
					('t 0)
				)
		)
		;;;;

		; Se o dividendo for infinito ou -infinito 
		; Cai nos casos 18,19,20,21 
		; Porque os outros foram tratados acima
		((equal X1 'INFINITO) (cond 
									((ehNeg X2) '-INFINITO)
									('t 'INFINITO)
							   )
		) 
		
		((equal X1 '-INFINITO) (cond 
									((ehNeg X2) 'INFINITO)
									('t '-INFINITO)
							   )
		)

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;;;;;
		; DIVISÃO NORMAL
		((and (ehNeg X1) (ehNeg X2)) (divAux (inverter X1) (inverter X2)))
		((and (ehNeg X1) (not (ehNeg X2))) (inverter (divAux (inverter X1) X2)))
		((and (not (ehNeg X1)) (ehNeg X2)) (inverter (divAux X1 (inverter X2))))
		('t (divAux X1 X2))

	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POTENCIAÇÃO

; 27.	0 ^ n = ∞, se n<0 		ok 
; 28.	n ^ ∞ = NaN, se n≠0		ok
; 29.	n ^ -∞ = NaN, se n≠0	ok
; 30.	0 ^ ∞ = 0 				ok
; 31.	0 ^ -∞ = 0				ok
; 32.	0 ^ 0 = NaN 			ok 
; 33.	∞ ^ n = ∞, se n>0 		ok
; 34.	∞ ^ n = 0, se n<0		ok
; 35.	∞ ^ 0 = NaN 			ok 
; 36.	-∞ ^ n = NAN 			ok
; 37.	n ^ ∞ = ∞, se n>1[nan]	ok
; 38.	n ^ -∞ = 0[nan]			ok
; 39.	∞ ^ ∞ = ∞				ok
; 40.	∞ ^ -∞ = NAN 			ok
; 41.	-∞ ^ ∞ = NAN 			ok
; 42.	-∞ ^ -∞ = NaN 			ok

(defun potAux (X1 X2)
	(cond
		((equal X2 0) 1)
		('t (mult X1 (potAux X1 (1- X2))))
	)
)

(defun pot (X1 X2)
	(cond 
		((equal X2 0) 'NAN) ; Caso 32 e 35
		((and (equal X1 0) (ehNeg X2)) 'INFINITO) ; Caso 27
		((equal X1 0) (or (equal X2 'INFINITO) (equal X2 '-INFINITO)) 0)   ; Caso 30, 31
		((and (equal X2 'INFINITO) (not (or (equal X1 'INFINITO) (equal X1 '-INFINITO)))) 'NAN) ; Caso 28 e 37
		((and (equal X2 '-INFINITO) (not (or (equal X1 'INFINITO) (equal X1 '-INFINITO)))) 'NAN) ; Caso 29 e 38
		((and (equal X1 'INFINITO) (equal X2 'INFINITO)) 'INFINITO) ; Caso 39
		((or (equal X2 'INFINITO) (equal X2 '-INFINITO)) 'NAN)    ; Caso 40,41 e 42
		((and (equal X1 'INFINITO) (ehNeg X2)) 0)    ; Case 34 
		((and (equal X1 'INFINITO) (not (ehNeg X2))) 'INFINITO) ; Caso 33
		((equal X1 '-INFINITO) 'NAN) ; Caso 36


		;;;;;;;;;;
		;; POTENCIA NORMAL
		('t (potAux X1 X2))
			
	)
)


(defun modulo (X1 X2)
	(cond 
		((and (ehNeg X1) (ehNeg X2)) (moduloAux (inverter X1) (inverter X2)))
		((and (ehNeg X1) (not (ehNeg X2))) (inverter (moduloAux (inverter X1) X2)))
		((and (not (ehNeg X1)) (ehNeg X2)) 'CONTA_INVALIDA) 
		('t (moduloAux X1 X2))
	)
)

(defun moduloAux (X1 X2)
	(cond 
		((maiorQue X2 X1) X1)
		('t (modulo (somarSubtrair (inverter X2) X1) X2))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolverP (E)
	(cond
		; Resolver um elemento não atomico, concatena o resultado dele com o resto da lista
		((not (atom (car E))) (resolverP (cons (car (expressao (car E))) (cdr E))))
		; Se for nil, acabou a lista
		((equal (cdr E) 'nil) E)
		; Se for atomico pula 
		((atom (car E)) (cons (car E) (resolverP (cdr E))))
		
	)
)

(defun resolverE (L)
	(cond 
		
		((atom L) L)
		((equal (car (cdr L)) '^) 
			(cond 
				
				; Calcula , concatena e continua percorrendo
				((atom (car (cdr (cdr L)))) (resolverE (cons (pot (car L) (car (cdr (cdr L)))) (cdr (cdr (cdr L))))))

				; É um outro grupo de parenteses
				('t (pot (car L) (expressao (car (cdr (cdr L))))))
			)
		)
		((equal (car (cdr L)) '%) 
			(cond 
				
				; Calcula , concatena e continua percorrendo
				((atom (car (cdr (cdr L)))) (resolverE (cons (modulo (car L) (car (cdr (cdr L)))) (cdr (cdr (cdr L))))))

				; É um outro grupo de parenteses
				('t (modulo (car L) (expressao (car (cdr (cdr L))))))
			)
		)

		; Se não for sinal de ^ continuar verificando a lista

		((equal (cdr L) 'nil) L) ; Se o cdr da lista der nil, quer dizer que esta lista acabou 
		
		('t (cons (car L)(resolverE (cdr L))))
	)
)

(defun resolverMD (L)
	(cond 

		((atom L) L)
		((equal (car (cdr L)) '*) 
			(cond 
				
				; Calcula , concatena e continua percorrendo
				((atom (car (cdr (cdr L)))) (resolverMD (cons (mult (car L) (car (cdr (cdr L)))) (cdr (cdr (cdr L))))))

				; É um outro grupo de parenteses
				('t (mult (car L) (expressao (car (cdr (cdr L))))))
			)
		)
		; Se não for sinal de * continuar verificando a lista
		((equal (car (cdr L)) '/) 
			(cond 
				
				; Calcula , concatena e continua percorrendo
				((atom (car (cdr (cdr L)))) (resolverMD (cons (div (car L) (car (cdr (cdr L)))) (cdr (cdr (cdr L))))))

				; É um outro grupo de parenteses
				('t (div (car L) (expressao (car (cdr (cdr L))))))
			)
		)

		((equal (cdr L) 'nil) L) ; Se o cdr da lista der nil, quer dizer que esta lista acabou 
		('t (cons (car L)(resolverMD (cdr L))))
	)
)

(defun resolverAS (L)
	(cond 

		((atom L) L)
		((equal (car (cdr L)) '+) 
			(cond 
				
				; Calcula , concatena e continua percorrendo
				((atom (car (cdr (cdr L)))) (resolverAS (cons (somarSubtrair (car L) (car (cdr (cdr L)))) (cdr (cdr (cdr L))))))

				; É um outro grupo de parenteses
				('t (somarSubtrair (car L) (expressao (car (cdr (cdr L))))))
			)
		)
		; Se não for sinal de * continuar verificando a lista
		((equal (car (cdr L)) '-) 
			(cond 
				
				; Calcula , concatena e continua percorrendo
				((atom (car (cdr (cdr L)))) (resolverAS (cons (somarSubtrair (car L) (inverter(car (cdr (cdr L))))) (cdr (cdr (cdr L))))))

				; É um outro grupo de parenteses
				('t (somarSubtrair (car L) (expressao (car (cdr (cdr L))))))
			)
		)

		((equal (cdr L) 'nil) L) ; Se o cdr da lista der nil, quer dizer que esta lista acabou 
		('t (cons (car L)(resolverAS (cdr L))))
	)
)

(defun expressao (X)
	(cond ((atom X) X)
		  ((ehValido X) (resolverAS 
		  		(resolverMD 
		  			(resolverE 
		  				(resolverP X)
		  			)
		  		)
		  	   )
		  )
		  ('t 'EXPRESSAO_INVALIDA)
 	)
 )