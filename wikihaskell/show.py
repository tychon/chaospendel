#/usr/bin/env python

#Import Modules
import os, pygame, math, time
from pygame.locals import *
if not pygame.font: print 'Warning, fonts disabled'

#classes for our game objects
class Pendulum(pygame.sprite.Sprite):
  def __init__(self, screen, maxtotalradius):
    pygame.sprite.Sprite.__init__(self) #call Sprite initializer
    self.screen = screen
    self.image = pygame.Surface(screen.get_size())
    self.rect = self.image.get_rect()
    self.image.fill( (0, 0, 0) )
    self.l = self.rect.width/5
  
  def setAngles(self, angles):
    self.angles = angles
  
  def update(self):
    self.image.fill( (0, 0, 0) )
    middle = self.rect.width/2
    x1 = middle + self.l * math.sin(self.angles[0])
    y1 = middle + self.l * -math.cos(self.angles[0])
    x2 = x1 + self.l * math.sin(self.angles[1])
    y2 = y1 + self.l * -math.cos(self.angles[1])
    pygame.draw.line(self.image, (250, 0, 0), (middle, middle), (x1, y1) )
    pygame.draw.line(self.image, (250, 0, 0), (x1, y1), (x2, y2) )

def main():
  #Initialize Everything
  pygame.init()
  screen = pygame.display.set_mode((200, 200))
  pygame.display.set_caption('Chaos')
  pygame.mouse.set_visible(0)
  
  # open file
  filename = "out.txt"
  f = open(filename, "r")
  
  #Prepare Game Objects
  clock = pygame.time.Clock()
  pend = Pendulum(screen, 200)
  allsprites = pygame.sprite.RenderPlain((pend))
  
  artifitial_time = 0
  time_step = 0.001
  font = pygame.font.Font(None, 18)
  starttime = time.clock()
  framenum = 0
  #Main Loop
  while True:
    clock.tick(1000)
    
    #Handle Input Events
    for event in pygame.event.get():
      if event.type == QUIT: # react to normal window closing action
        return
    
    # load line from file
    numberstrs = f.readline().split(",")
    if len(numberstrs[0]) is 0: return
    ang = [float(x) for x in numberstrs ]
    pend.setAngles([ang[1], ang[3]])
    
    #Draw Everything
    allsprites.update()
    #screen.blit(background, (0, 0))
    allsprites.draw(screen)
    # draw font
    text1 = font.render("t/s="+str(artifitial_time), 1, (0, 250, 0))
    text2 = font.render("fps="+str(int(framenum/(time.clock()-starttime+0.00001))), 1, (0, 250, 0))
    screen.blit(text1, (0, 0) )
    screen.blit(text2, (0, 15) )
    pygame.display.flip()
    
    artifitial_time = artifitial_time + time_step
    framenum = framenum +1

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()
