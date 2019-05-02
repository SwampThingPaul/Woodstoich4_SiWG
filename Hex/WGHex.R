library(hexSticker)

#Original version
imgurl="D:/UF/WoodStoich19/Si_WG/Hex/3d_coast_v2.png"
sticker(imgurl, package="Silicon\nWorking Group", 
        p_size=17, s_x = 1, s_y = 1.16, s_width = 0.95, s_height = 0.95,p_y=1.1,
        h_fill = "#ffffff", p_color  = "#000000", h_color = "#000000",
        p_family="serif",filename="hex/woodstoich_Si.png")


## Hacked version

library(showtext)
font_add("hippy","D:/UF/WoodStoich19/Si_Wg/Hex/GoodVibrations-Regular.otf")
font_families()

library(ggplot2)
library(ggimage)
library(hexSticker)
library(magick)

fill="white";#"#000000"
color="black";#"#ffffff"
subplot=imgurl
size=0.8
s_x=1
s_y=1.1
s_width=0.90
t_x=1
t_y=0.5
text.val="Silicon\nWorking Group"
t_color="black"
font.val="serif"
t_size=15

hexd <- data.frame(x = 1 + c(rep(-sqrt(3)/2, 2), 0, rep(sqrt(3)/2,2), 0), y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1))
hexd <- rbind(hexd, hexd[1, ])
d <- data.frame(x = s_x, y = s_y, image = subplot)
d.txt <- data.frame(x = t_x, y = t_y, label = text.val)

#windows()
hex.stick=
  ggplot()+
  geom_polygon(aes_(x = ~x, y = ~y), data = hexd, size = size,fill = fill, color = color)+
  geom_image(aes_(x = ~x, y = ~y, image = ~image),d, size = s_width)+ 
  theme_sticker(size)+
  geom_text(aes_(x = 1, y = 1.6, label = "Silicon"),size = t_size, color = t_color, family = font.val )+
  geom_text(aes_(x = 1, y = 0.65, label = "Working Group"),size = t_size, color = t_color, family = font.val)+
  geom_text(aes_(x = 1.5, y = 0.25, label = "Original graphic by Tracey Saxby"),angle=30,family=font.val,fontface="plain",color="black",size=3)+
  #geom_text(aes_(x = 0.6, y = 1.71, label = "Bulseco-McKim, Carey, Sethna, Thomas, Julian"),angle=30,family=font.val,fontface="italic",color="black",size=3)+
  geom_text(aes_(x = 0.5, y = 0.35, label = "Woodstoich 4"),angle=-30,family="hippy",fontface="plain",color="black",size=7)
  
ggsave(hex.stick, width = 43.9, height = 50.8, 
       filename = "D:/UF/WoodStoich19/Si_Wg/Hex/woodstoich_Si_Hex.png", 
       bg = "transparent", units = "mm", dpi=300)

## turn showtext off if no longer needed
showtext_auto(FALSE) 
