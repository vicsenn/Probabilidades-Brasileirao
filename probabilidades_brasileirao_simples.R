###################################################################################################################
#
# ROTINA PARA GERAR PROBABILIDADES DOS CAMPEÕES DO BRASILEIRA - SERIE A
# Rotina em R
#
# AUTOR: VICTOR MAIA S. DELGADO (UFOP)
# e-mail: victor.delgado@ufop.edu.br (Victor)
# DATA da última versão: 21/11/2021
# CPU 1:      Desktop HP; processor i7-3770 @ 3.40GHz, RAM: 16.0 GB
#  OS 1:      Windows 7 Professional
# CPU 2:      ACER Aspire E5-573G; processor i7-5500U @ 2.40GHz, RAM: 8.0 GB
#  OS 2:      Windows 10 Home Single Language
# Versão R: 4.0.5(x64) 
#
# OBS.1: Rotina para gera as chances dos times de acordo com técnicas naïve, 
# 		 os pareamentos (qual time enfrenta qual) não importam.
#		 Para maiores detalhes ou dúvidas entre em contato com o autor que aparece no e-mail acima.
# OBS.2: Essa Rotina foi gerada primordialmente no R 4.0.5 para Windows 7, 64 Bits, para rodá-la em outros sistemas operacionais alguns detalhes devem ser observados.
#
###################################################################################################################

# DADOS DE ENTRADA IMPORTANTES:

# Ordem alfabética dos times:

times <- c('America', 'Atletico GO', 'Atletico', 'Athletico', 'Bahia', 'Bragantino', 'Ceara', 'Chapecoense', 'Corinthians', 'Cuiaba',
	'Flamengo', 'Fluminense', 'Fortaleza', 'Gremio', 'Internacional', 'Juventude', 'Palmeiras', 'Santos', 
	'Sao Paulo', 'Sport')

# Abreviatura dos times:

abr <- c('AME', 'ACG', 'CAM', 'CAP', 'BAH', 'BGT', 'CEA', 'CHA', 'COR', 'CUI', 'FLA', 'FLU', 'FOR', 'GRE', 'INT', 'JUV', 'PAL', 'SAN', 'SAO', 'SPT')

# Pontos na rodada atual de acordo com a oderm alfabética

pontos <- c(45, 40, 74, 41, 37, 52, 46, 15, 53, 43, 66, 48, 52, 35, 47, 39, 58, 42, 41, 33)

# Vamos fazer uma tabela de acordo com a classificação:

tab <- data.frame(matrix(c(abr, pontos), nrow = 20, ncol = 2))
names(tab) <- c("equipe", "pontos")

# Para ordenar pelos pontos,
# Ainda não tem critério de desempate:

# AQUI há mais imputs importantes, repare que já está quase na ordem da tabela porém sem critério de desempate.

tab <- tab[order(tab[,2], decreasing = TRUE),]
rodadas <- c(33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 33, 33, 33, 33, 33, 33, 34, 34)
vitorias <- c(23, 20, 18, 14, 13, 15, 13, 12, 10, 11, 9, 10, 12, 9, 9, 9, 9, 10, 8, 1)
empates <- c(5, 7, 12, 9, 13, 7, 9, 11, 16, 12, 16, 12, 5, 14, 13, 12, 10, 5, 9, 12)
derrotas <- rodadas - (vitorias + empates)

# Juntando na tabela:

tab <- cbind(tab, rodadas, vitorias, empates, derrotas)

# Reordenando com critério de vitórias como desempate:

tab <- tab[order(tab[,2], tab[,4], decreasing = TRUE),]
rownames(tab) <- 1:nrow(tab)

# Em alguns casos o critério de vitórias não é suficiente então para o Saldo de Gols, mais dois vetores de imputs:
# Atenção à ordem dos dados:

GP <- c(53, 64, 52, 37, 41, 50, 33, 42, 36, 35, 31, 30, 39, 26, 25, 32, 33, 33, 21, 27) # Gols Pro
GC <- c(22, 29, 40, 32, 41, 40, 34, 37, 34, 36, 32, 38, 43, 33, 33, 39, 43, 42, 33, 59) # Gols Contra
Saldo <- GP - GC

# Juntando na tabela do brasileirão:

tab <- cbind(tab, GP, GC, Saldo)

# Corrigindo o fato de que o valor dos pontos não ficou como número na minha tabela:

tab[,2] <- as.numeric(tab[,2])

# O aproveitamento pode ser obtido a partir dos dados:

Aproveitamento <- round(tab[,2]/(3*tab[,3]),3)
tab <- cbind(tab, Aproveitamento)

###################################################
#
# Vamos fazer listas das rodadas que ainda faltam:
#
###################################################

# Rodadas que ainda faltam (inclui as adiadas)

rodada_02 <- list(c('GRE', 'FLA', 'EMP'))

rodada_30 <- list(c('ACG', 'JUV', 'EMP'))

rodada_32 <- list(c('BAH', 'CAM', 'EMP'))

rodada_34 <- list(c('SAO', 'CAP', 'EMP'))

rodada_35 <- list(c('PAL', 'CAM', 'EMP'),
				  c('FLU', 'INT', 'EMP'),
				  c('SAN', 'FOR', 'EMP'),
				  c('CEA', 'COR', 'EMP'),
				  c('ACG', 'BAH', 'EMP'),
				  c('JUV', 'BGT', 'EMP'),
				  c('AME', 'CHA', 'EMP'),
				  c('GRE', 'SAO', 'EMP'),
				  c('CAP', 'CUI', 'EMP'),
				  c('SPT', 'FLA', 'EMP')
				  )

rodada_36 <- list(c('BAH', 'GRE', 'EMP'),
				  c('CHA', 'ACG', 'EMP'),
				  c('BGT', 'AME', 'EMP'),
				  c('SAO', 'SPT', 'EMP'),
				  c('COR', 'CAP', 'EMP'),
				  c('CAM', 'FLU', 'EMP'),
				  c('INT', 'SAN', 'EMP'),
				  c('FLA', 'CEA', 'EMP'),
				  c('CUI', 'PAL', 'EMP'),
				  c('FOR', 'JUV', 'EMP')
				  )

rodada_37 <- list(c('COR', 'GRE', 'EMP'),
				  c('CAM', 'BGT', 'EMP'),
				  c('BAH', 'FLU', 'EMP'),
				  c('INT', 'ACG', 'EMP'),
				  c('CEA', 'AME', 'EMP'),
				  c('SAO', 'JUV', 'EMP'),
				  c('CAP', 'PAL', 'EMP'),
				  c('FLA', 'SAN', 'EMP'),
				  c('CHA', 'SPT', 'EMP'),
				  c('CUI', 'FOR', 'EMP')
				  )

rodada_38 <- list(c('FLU', 'CHA', 'EMP'),
				  c('PAL', 'CEA', 'EMP'),
				  c('SAN', 'CUI', 'EMP'),
				  c('AME', 'SAO', 'EMP'),
				  c('GRE', 'CAM', 'EMP'),
				  c('FOR', 'BAH', 'EMP'),
				  c('SPT', 'CAP', 'EMP'),
				  c('BGT', 'INT', 'EMP'),
				  c('JUV', 'COR', 'EMP'),
				  c('ACG', 'FLA', 'EMP')
				  )

#####################################################
#
# Simulação Naïve (ingênua), 
# Considera as chances de vitória, derrota e empate 
# como sendo 1/3 para cada um dos times:
#
#####################################################

iter <- list(rodada_02, rodada_30, rodada_32, rodada_34, rodada_35, rodada_36, rodada_37, rodada_38)

# Número de simulações:

n <- 13000

# Note que quanto mais rodadas precisarem ser simuladas o ideal é que esse número 'n' aumente de acordo.

resultados_naive <- matrix(0, nrow = 20, ncol = n)

for(i in 1:n)
{
	# Uma tabela sem GP, SC e Saldo nos será mais útil no momento:

	tab2 <- tab[,-c(7,8,9,10)]

	for(j in 1:8)
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
tab3 <- tab2[order(tab2[,2], tab2[,4], decreasing = TRUE),]
resultados_naive[,i]  <- tab3[,1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

chances_campeao <- (table(resultados_naive[1,])/n)*100
barplot(chances_campeao, main = "Probabilidades de Campeão Serie A Brasileiro 2021", ylim = c(0,110))
text(0.7, 102, "98%")
text(1.9, 5, "2%")

# Para ver as chances de ficar entre os 6 primeiros colocados:
# Olhar os times que ficam entre os 6 primeiros (Libertadores e Pré-liberda, independente da Copa do Brasil)

chances_libertadores <- sort((table(resultados_naive[1:6,])/n)*100, decreasing = TRUE)
barplot(chances_libertadores, main = "Probabilidades de Ficar entre os 6 primeiros Serie A Brasileiro 2021", ylim = c(0,110))

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento <- sort((table(resultados_naive[17:20,])/n)*100, decreasing = TRUE)
barplot(chances_rebaixamento, main = "Probabilidades de Ficar entre os 4 últimos Serie A Brasileiro 2021", ylim = c(0,110))

## FIM DA ROTINA!