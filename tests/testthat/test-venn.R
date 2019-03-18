context("Venn")

test_that("Venn",
          {
                ### R Output as an input
                # Simple example
                r.output <- list(
                    list("sets"= list(0), "label"= "Like", "size"= 100),
                    list("sets"= list(1), "label"= "Love", "size"= 50),
                    list("sets"= list(2), "label"= "Dislike", "size"= 100),
                    list("sets"= list(3), "label"= "Hate", "size"= 50),
                    list("sets"= list(0, 1), "size"= 50),
                    list("sets"= list(0, 2), "size"= 0),
                    list("sets"= list(2, 3), "size"= 50))
                Venn(r.output)
                # More complicated example
                r.output <- list(
                        list("sets"= list(0), "label"= "Radiohead", "size"= 77348),
                        list("sets"= list(1), "label"= "Thom Yorke", "size"= 5621),
                        list("sets"= list(2), "label"= "John Lennon", "size"= 7773),
                        list("sets"= list(3), "label"= "Kanye West", "size"= 27053),
                        list("sets"= list(4), "label"= "Eminem", "size"= 19056),
                        list("sets"= list(5), "label"= "Elvis Presley", "size"= 15839),
                        list("sets"= list(6), "label"= "Explosions in the Sky", "size"= 10813),
                        list("sets"= list(7), "label"= "Bach", "size"= 9264),
                        list("sets"= list(8), "label"= "Mozart", "size"= 3959),
                        list("sets"= list(9), "label"= "Philip Glass", "size"= 4793),
                        list("sets"= list(10), "label"= "St. Germain", "size"= 4136),
                        list("sets"= list(11), "label"= "Morrissey", "size"= 10945),
                        list("sets"= list(12), "label"= "Outkast", "size"= 8444),
                        list("sets"= list(0, 1), "size"= 4832),
                        list("sets"= list(0, 2), "size"= 2602),
                        list("sets"= list(0, 3), "size"= 6141),
                        list("sets"= list(0, 4), "size"= 2723),
                        list("sets"= list(0, 5), "size"= 3177),
                        list("sets"= list(0, 6), "size"= 5384),
                        list("sets"= list(0, 7), "size"= 2252),
                        list("sets"= list(0, 8), "size"= 877),
                        list("sets"= list(0, 9), "size"= 1663),
                        list("sets"= list(0, 10), "size"= 899),
                        list("sets"= list(0, 11), "size"= 4557),
                        list("sets"= list(0, 12), "size"= 2332),
                        list("sets"= list(1, 2), "size"= 162),
                        list("sets"= list(1, 3), "size"= 396),
                        list("sets"= list(1, 4), "size"= 133),
                        list("sets"= list(1, 5), "size"= 135),
                        list("sets"= list(1, 6), "size"= 511),
                        list("sets"= list(1, 7), "size"= 159),
                        list("sets"= list(1, 8), "size"= 47),
                        list("sets"= list(1, 9), "size"= 168),
                        list("sets"= list(1, 10), "size"= 68),
                        list("sets"= list(1, 11), "size"= 336),
                        list("sets"= list(1, 12), "size"= 172),
                        list("sets"= list(2, 3), "size"= 406),
                        list("sets"= list(2, 4), "size"= 350),
                        list("sets"= list(2, 5), "size"= 1335),
                        list("sets"= list(2, 6), "size"= 145),
                        list("sets"= list(2, 7), "size"= 347),
                        list("sets"= list(2, 8), "size"= 176),
                        list("sets"= list(2, 9), "size"= 119),
                        list("sets"= list(2, 10), "size"= 46),
                        list("sets"= list(2, 11), "size"= 418),
                        list("sets"= list(2, 12), "size"= 146),
                        list("sets"= list(3, 4), "size"= 5465),
                        list("sets"= list(3, 5), "size"= 849),
                        list("sets"= list(3, 6), "size"= 724),
                        list("sets"= list(3, 7), "size"= 273),
                        list("sets"= list(3, 8), "size"= 143),
                        list("sets"= list(3, 9), "size"= 180),
                        list("sets"= list(3, 10), "size"= 218),
                        list("sets"= list(3, 11), "size"= 599),
                        list("sets"= list(3, 12), "size"= 3453),
                        list("sets"= list(4, 5), "size"= 977),
                        list("sets"= list(4, 6), "size"= 232),
                        list("sets"= list(4, 7), "size"= 250),
                        list("sets"= list(4, 8), "size"= 166),
                        list("sets"= list(4, 9), "size"= 97),
                        list("sets"= list(4, 10), "size"= 106),
                        list("sets"= list(4, 11), "size"= 225),
                        list("sets"= list(4, 12), "size"= 1807),
                        list("sets"= list(5, 6), "size"= 196),
                        list("sets"= list(5, 7), "size"= 642),
                        list("sets"= list(5, 8), "size"= 336),
                        list("sets"= list(5, 9), "size"= 165),
                        list("sets"= list(5, 10), "size"= 143),
                        list("sets"= list(5, 11), "size"= 782),
                        list("sets"= list(5, 12), "size"= 332),
                        list("sets"= list(6, 7), "size"= 262),
                        list("sets"= list(6, 8), "size"= 85),
                        list("sets"= list(6, 9), "size"= 284),
                        list("sets"= list(6, 10), "size"= 68),
                        list("sets"= list(6, 11), "size"= 363),
                        list("sets"= list(6, 12), "size"= 218),
                        list("sets"= list(7, 8), "size"= 1581),
                        list("sets"= list(7, 9), "size"= 716),
                        list("sets"= list(7, 10), "size"= 133),
                        list("sets"= list(7, 11), "size"= 254),
                        list("sets"= list(7, 12), "size"= 132),
                        list("sets"= list(8, 9), "size"= 280),
                        list("sets"= list(8, 10), "size"= 53),
                        list("sets"= list(8, 11), "size"= 117),
                        list("sets"= list(8, 12), "size"= 67),
                        list("sets"= list(9, 10), "size"= 57),
                        list("sets"= list(9, 11), "size"= 184),
                        list("sets"= list(9, 12), "size"= 89),
                        list("sets"= list(10, 11), "size"= 51),
                        list("sets"= list(10, 12), "size"= 115),
                        list("sets"= list(11, 12), "size"= 162),
                        list("sets"= list(0, 1, 6), "size"= 480),
                        list("sets"= list(0, 1, 9), "size"= 152),
                        list("sets"= list(0, 2, 7), "size"= 112),
                        list("sets"= list(0, 3, 4), "size"= 715),
                        list("sets"= list(0, 3, 12), "size"= 822),
                        list("sets"= list(0, 4, 5), "size"= 160),
                        list("sets"= list(0, 5, 11), "size"= 292),
                        list("sets"= list(0, 6, 12), "size"= 122),
                        list("sets"= list(0, 7, 11), "size"= 118),
                        list("sets"= list(0, 9, 10), "size" =13),
                        list("sets"= list(2, 7, 8), "size"= 72))
                Venn(r.output)
                # Setting font size
                Venn(r.output, data.label.font.size = 6)
                # Numeric inptuts
                data("cola", package = "flipExampleData")
                x <- cola[,c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F")]
                x <- suppressWarnings(flipData::TidyRawData(x, as.numeric = TRUE))
                x[x < 4] <- 0
                x[x != 0] <- 1
                # Numeric input - 2D
                Venn(x[,1:2])
                # logical input
                zx = as.data.frame(x[,1:2] >= 1)
                library(flipFormat)
                Labels(zx) = Labels(x)[1:2]
                Venn(zx)
                # Numeric input - 3D
                Venn(x[,1:3])
                # Numeric input - 4D
                Venn(x[,1:4])
                # Numeric input - 5D
                Venn(x[,1:5])
                # Numeric input - 6D
                Venn(x[,1:6])
                # Numeric input - 1D
                expect_error(Venn(x[,1]))
                # Factor input
                expect_error(Venn(data.frame(sapply(x, as.factor))))
          })

