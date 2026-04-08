############################################################################################################################################################################
#
# ROTINA PARA GERAR PROBABILIDADES DOS CAMPEÕES NO BRASILEIRÃO SÉRIE A
# Scripts para Calcular as Probabilidades de Vitória dos times 
#
# Rotina em R
#
# AUTOR: VICTOR MAIA S. DELGADO (UFOP)
# e-mail: victor.delgado@ufop.edu.br (Victor)
# DATA da última versão: 08/04/2026
# CPU: ASUS; processor intel i5 12th Gen Intel© Core™ i5-12400F × 6, @ 2.40GHz, RAM: 32.0 GB
# Versão R: 4.5.2(x64) "[Not] Part in a Rumble"
#
# OBS.1: 
#   Essa Rotina foi gerada primordialmente no R 4.5.2 para Linux Mint 22.3 (Zena), 64 Bits, para rodá-la em outros SOs alguns detalhes devem ser observados.
# OBS.2:
#
#   Deixando aqui apenas o cálculo naïve. Em ocasiões anteriores eu havia feito um código com probabilidades diferentes de vitórias para os diferentes times.
#   Isso funciona melhor nas rodadas finais do campeonato, nas demais fases resultados de Vitória, Empate e Derrota equiprováveis são boas o suficiente.
#   Além de acrescentarem uma variabilidade boa e próxima de uma incerteza correta para campeonatos de futebol.
#
###############################################################################################################################################################################

# DADOS DE ENTRADA IMPORTANTES:

# Ordem alfabética dos times:

times <- c('Atletico', 'Athletico', 'Bahia', 'Botafogo', 'Bragantino', 'Chapecoense', 'Corinthians', 'Coritiba', 'Cruzeiro', 'Flamengo', 
		   'Fluminense', 'Gremio', 'Internacional', 'Mirassol', 'Palmeiras', 'Remo', 'Santos', 'Sao Paulo', 'Vasco', 'Vitoria')

# Abreviatura dos times:

abr <- c('CAM', 'CAP', 'BAH', 'BOT', 'BGT', 'CHP', 'COR', 'CTB', 'CRU', 'FLA', 'FLU', 'GRE', 'INT', 'MIR', 'PAL', 'REM', 'SAN', 'SAO', 'VAS', 'VIT')

# Pontos na rodada atual de acordo com a oderm alfabética

pontos <- c(14, 16, 17, 12, 14, 8, 10, 15, 7, 17, 20, 12, 12, 6, 25, 7, 10, 20, 12, 11)

# Vamos fazer uma tabela de acordo com a classificação:

tab <- data.frame(equipe = abr, pontos = pontos)

# Para ordenar pelos pontos,
# Ainda não tem critério de desempate:

# AQUI há mais imputs importantes, repare que já está quase na ordem da tabela porém sem critério de desempate.

tab <- tab[order(tab[,2], decreasing = TRUE),]
rodadas <- c(10, 10, 10, 9, 9, 10, 10, 10, 10, 9, 10, 10, 10, 9, 10, 10, 9, 10, 10, 9)
vitorias <- c(8, 6, 6, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1)
empates <- c(1, 2, 2, 2, 2, 1, 3, 2, 2, 0, 3, 3, 3, 2, 4, 4, 5, 4, 4, 3)
derrotas <- rodadas - (vitorias + empates)

# Juntando na tabela:

tab <- cbind(tab, rodadas, vitorias, empates, derrotas)

# Reordenando com critério de vitórias como desempate:

tab <- tab[order(tab[,2], tab[,4], decreasing = TRUE),]
rownames(tab) <- 1:nrow(tab)

# Em alguns casos o critério de vitórias não é suficiente então para o Saldo de Gols, mais dois vetores de inputs:
# Atenção à ordem dos dados:

GP <- c(21, 17, 15, 13, 16, 15, 11, 14, 10, 16, 14, 9, 15, 9, 8, 13, 10, 12, 10, 10) # Gols Pro
GC <- c(10, 11, 7, 9, 9, 13, 10, 12, 10, 19, 14, 10, 16, 14, 11, 16, 16, 20, 17, 14) # Gols Contra
Saldo <- GP - GC

# Juntando na tabela do brasileirão:

tab <- cbind(tab, GP, GC, Saldo)

# E mais um reordenamento

tab <- tab[order(tab[,2], tab[,4], tab[,9], tab[,7], decreasing = TRUE),]

# O aproveitamento pode ser obtido a partir dos dados:

Aproveitamento <- round(tab[,2]/(3*tab[,3]),3)
tab <- cbind(tab, Aproveitamento)

###################################################
#
# Vamos fazer listas das rodadas que ainda faltam: ATUALIZAR DEPOIS
#
###################################################

# Rodadas que ainda faltam (inclui as adiadas)

rodada_38 <- list(c('GOI', 'AME', 'EMP'),
				  c('FLU', 'GRE', 'EMP'),
				  c('VAS', 'BGT', 'EMP'),
				  c('SAO', 'FLA', 'EMP'),
				  c('SAN', 'FOR', 'EMP'),
				  c('CRU', 'PAL', 'EMP'),
				  c('INT', 'BOT', 'EMP'),
				  c('CTB', 'COR', 'EMP'),
				  c('BAH', 'CAM', 'EMP'),
				  c('CUI', 'CAP', 'EMP')
				  )

#####################################################
#
# Simulação Naïve (ingênua), 
# Considera as chances de vitória, derrota e empate 
# como sendo 1/3 para cada um dos times:
#
#####################################################

iter <- list(rodada_38)

# Número de simulações:

n <- 13000

# Note que quanto mais rodadas precisarem ser simuladas o ideal é que esse número 'n' aumente de acordo.

resultados_naive <- matrix(0, nrow = 20, ncol = n)

for(i in 1:n)
{
	# Uma tabela sem GP, SC e Saldo nos será mais útil no momento:

	tab2 <- tab[,-c(7,8,9,10)]
	desempate_1 <- rbinom(n = 20, size = 1, prob = 0.5)
	desempate_2 <- rbinom(n = 20, size = 1, prob = 0.5)
	tab2[,7:8] <- cbind(desempate_1, desempate_2)

	for(j in 1)
	{
	result <- lapply(iter[[j]], sample, size = 3)
		for(k in 1:length(result)){
			if(result[[k]][1] == "EMP"){
				tab2[tab2[,1] == result[[k]][2], 2:6] <- tab2[tab2[,1] == result[[k]][2], 2:6] + c(1,1,0,1,0)
				tab2[tab2[,1] == result[[k]][3], 2:6] <- tab2[tab2[,1] == result[[k]][3], 2:6] + c(1,1,0,1,0)
			}else{
				ganhou <- result[[k]][1]
				tab2[tab2[,1] == ganhou, 2:6] <- tab2[tab2[,1] == ganhou, 2:6] + c(3, 1, 1, 0, 0)
				perdeu <- which(result[[k]] == "EMP")
				perdeu <- result[[k]][-perdeu]
				perdeu <- perdeu[2]
				tab2[tab2[,1] == perdeu, 2:6] <- tab2[tab2[,1] == perdeu, 2:6] + c(0, 1, 0, 0, 1)
			}
		}
	}
tab3 <- tab2[order(tab2[,2], tab2[,4], tab2[,7], tab2[,8], decreasing = TRUE),]
resultados_naive[,i]  <- tab3[,1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

png(Campeao_brasileirao_23_v3.png)
chances_campeao <- (table(resultados_naive[1,])/n)*100
barplot(chances_campeao, main = "Probabilidades de Campeão Serie A Brasileiro 2023", ylim = c(0,110))
text(0.7, 10, "6.5%")
text(1.9, 27, "24%")
text(3.1, 9, "6%")
text(4.35, 5.5, "2.5%")
text(5.5, 64, "61%")
dev.off()

# Para ver as chances de ficar entre os 6 primeiros colocados:
# Olhar os times que ficam entre os 6 primeiros (Libertadores e Pré-liberda, independente da Copa do Brasil)

chances_libertadores <- sort((table(resultados_naive[1:6,])/n)*100, decreasing = TRUE)
barplot(chances_libertadores, main = "Probabilidades de Ficar entre os 6 primeiros Serie A Brasileiro 2023", ylim = c(0,110))

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento <- sort((table(resultados_naive[17:20,])/n)*100, decreasing = TRUE)
barplot(chances_rebaixamento, main = "Probabilidades de Ficar entre os 4 últimos Serie A Brasileiro 2023", ylim = c(0,110))
text(0.7, 102, "100%")
text(1.9, 102, "100%")
text(3.1, 102, "100%")
text(4.35, 57, "54%")
text(5.5, 32, "29%")
text(6.7, 18, "15%")
text(7.9, 5, "2%")
text(9.1, 3, "~0%")

## FIM DA ROTINA!
