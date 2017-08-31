barbara <-function (filename, species, ptm = FALSE, mutation=FALSE) {
  
  if (species == 0) 
  { fname <- read.table(file = 'Amphimedon+queenslandica-PTMs.txt', 
                        header = FALSE, sep = '\t', fill = TRUE)
  } else if (species == 1)
  {fname <- read.table(file = 'Anolis+carolinensis.gffPTMs.txt', 
                        header = FALSE, sep = '\t', fill = TRUE)
  } else if (species == 2)
  {fname <- read.table(file = 'Aplysia-PTMs.txt', fill = TRUE,
                       header = FALSE, sep = '\t')
  } else if (species == 3)
  {fname <- read.table(file = 'arabidopsis+thaliana-functional_data.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  } else if (species == 4)
  {fname <- read.table(file = 'Arbacia+punctulata-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 5)
  {fname <- read.table(file = 'Ashbya+gossypii.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 6)
  {fname <- read.table(file = 'Aspergillus+nidulans.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 7)
  {fname <- read.table(file = 'Bombina+maxima.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 8)
  {fname <- read.table(file = 'Bombina+orientalis.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 9)
  {fname <- read.table(file = 'Brachypodium+distachyon.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 10)
  {fname <- read.table(file = 'Branchiostoma+floridae-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 11)
  {fname <- read.table(file = 'Caenorhabditis+elegans-functional_data.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 12)
  {fname <- read.table(file = 'Canis+lupus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 13)
  {fname <- read.table(file = 'Cavia+porcellus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 14)
  {fname <- read.table(file = 'Chlamydomonas+reinhardtii.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 15)
  {fname <- read.table(file = 'Ciona+intestinalis-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 16)
  {fname <- read.table(file = 'Columba+livia+domestica.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 17)
  {fname <- read.table(file = 'Coprinus+cinereus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 18)
  {fname <- read.table(file = 'Cryptococcus+neoformans.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 19)
  {fname <- read.table(file = 'Daphnia+magna-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 20)
  {fname <- read.table(file = 'Dictyostelium+discoideum.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 21)
  {fname <- read.table(file = 'drosophila-functional_data.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 22)
  {fname <- read.table(file = 'Emiliania+huxleyi.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 23)
  {fname <- read.table(file = 'Escherichia+coli-functional_data.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 24)
  {fname <- read.table(file = 'Euprymna+scolopes-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 25)
  {fname <- read.table(file = 'Felis+catus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 27)
  {fname <- read.table(file = 'Galleria+mellonella-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 28)
  {fname <- read.table(file = 'Gallus+gallus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 29)
  {fname <- read.table(file = 'Gasterosteus+aculeatus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 30)
  {fname <- read.table(file = 'Gryllus+bimaculatus-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 31)
  {fname <- read.table(file = 'Herpes+simplex+virus-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 32)
  {fname <- read.table(file = 'Heterocephalus+glaber.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 33)
  {fname <- read.table(file = 'human-functional_data.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 34)
  {fname <- read.table(file = 'Hydra-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 35)
  {fname <- read.table(file = 'Lemna+gibba.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 36)
  {fname <- read.table(file = 'Loligo+pealei-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 37)
  {fname <- read.table(file = 'Lotus+japonicus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 38)
  {fname <- read.table(file = 'Medicago+truncatula.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 39)
  {fname <- read.table(file = 'Mesocricetus+auratus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 40)
  {fname <- read.table(file = 'Mimulus+guttatus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 41)
  {fname <- read.table(file = 'Mus+musculus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 42)
  {fname <- read.table(file = 'Mycobacterium+tuberculosis-functional_data.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 43)
  {fname <- read.table(file = 'Myotis+lucifugus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 44)
  {fname <- read.table(file = 'Nematostella+vectensis-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 45)
  {fname <- read.table(file = 'Neurospora+crassa-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 46)
  {fname <- read.table(file = 'Nicotiana+benthamiana.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 47)
  {fname <- read.table(file = 'Oikopleura+dioica-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 48)
  {fname <- read.table(file = 'Oryza+sativa.gffPTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 49)
  {fname <- read.table(file = 'Oryzias+latipes.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 50)
  {fname <- read.table(file = 'Petromyzon+marinus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 51)
  {fname <- read.table(file = 'Phage+lambda-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 52)
  {fname <- read.table(file = 'Phi+X+174-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 53)
  {fname <- read.table(file = 'Physcomitrella+patens.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 54)
  {fname <- read.table(file = 'Platynereis+dumerilii-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 55)
  {fname <- read.table(file = 'Poecilia+reticulata-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 56)
  {fname <- read.table(file = 'Pristionchus+pacificus-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 57)
  {fname <- read.table(file = 'Rattus+norvegicus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 58)
  {fname <- read.table(file = 'Rhesus+macaque.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 59)
  {fname <- read.table(file = 'Schizophyllum+commune.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 60)
  {fname <- read.table(file = 'Schizosaccharomyces+pombe.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 61)
  {fname <- read.table(file = 'Schmidtea+mediterranea-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 62)
  {fname <- read.table(file = 'Selaginella+moellendorffii.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 63)
  {fname <- read.table(file = 'Setaria+viridis.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 64)
  {fname <- read.table(file = 'Sigmodon+hispidus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 65)
  {fname <- read.table(file = 'SV40-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 66)
  {fname <- read.table(file = 'Symsagittifera+roscoffensis-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 67)
  {fname <- read.table(file = 'T4+phage-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 68)
  {fname <- read.table(file = 'Takifugu+rubripes.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 69)
  {fname <- read.table(file = 'Tetrahymena+thermophila.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 70)
  {fname <- read.table(file = 'Thalassiosira+pseudonana.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 71)
  {fname <- read.table(file = 'Tobacco+BY-2+cells.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 72)
  {fname <- read.table(file = 'Tobacco+mosaic+virus-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 73)
  {fname <- read.table(file = 'Tribolium+castaneum-PTMs.txt', 
                       header = FALSE, sep = '\t',fill = TRUE)
  }else if (species == 74)
  {fname <- read.table(file = 'uniprot-Aliivibrio+fischeri.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 75)
  {fname <- read.table(file = 'uniprot-Bacillus+subtilis.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 76)
  {fname <- read.table(file = 'uniprot-Caulobacter+crescentus.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 77)
  {fname <- read.table(file = 'uniprot-Mycoplasma+genitalium.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 78)
  {fname <- read.table(file = 'uniprot-Pseudomonas+fluorescens.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 79)
  {fname <- read.table(file = 'uniprot-Synechocystis.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 80)
  {fname <- read.table(file = 'Ustilago+maydis-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 81)
  {fname <- read.table(file = 'Xenopus+laevis.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 82)
  {fname <- read.table(file = 'Xenopus+tropicalis.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 83)
  {fname <- read.table(file = 'yeast-functional_data.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 84)
  {fname <- read.table(file = 'Zea+mays.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else if (species == 85)
  {fname <- read.table(file = 'Zebra+finch.gff-PTMs.txt', 
                       header = FALSE, sep = '\t', fill = TRUE)
  }else {
    print("please correct the species code and run UniMap again")
  }
  if (ptm == TRUE)
    { filename  <- read.table(file = filename, header = FALSE, sep = '\t')
    m = merge(filename, fname, by.x = "V1", by.y ="V1")
    write.table(m, file='Functional_region_PTM.txt', append =FALSE, sep = "\t", col.name = FALSE, quote = FALSE)
  
  } else if (mutation ==TRUE)
  { filename  <- read.table(file = filename, header = FALSE, sep = '\t',)
  m = merge(filename, fname, by.x = c("V1", "V2"), by.y=c("V1", "V3"))
  write.table(m,file='Functional_region_PTM.txt', append =FALSE, sep = "\t", col.name = FALSE, quote = FALSE)
  }
}