
import pygame
from pygame.locals import *

class Graph(pygame.sprite.Sprite):
  def __init__(self, rect, samples, minval, maxval, colors):
    pygame.sprite.Sprite.__init__(self)
    self.rect = rect
    self.image = pygame.Surface((rect.width, rect.height))
    self.minval = minval
    self.maxval = maxval
    self.samples = samples
    self.colors = colors
    self.scale = rect.height / float(maxval-minval)
    self.xstep = rect.width / float(samples)
    self.values = []
  
  def push_val(self, vals):
    self.values.append(vals)
    if len(self.values) > self.samples: self.values.pop(0)
  
  def update(self):
    self.image.fill( (0, 0, 0) )
    xpos = self.rect.width - len(self.values) * self.xstep
    lastYPosS = [0 for i in range(len(self.colors))]
    for vals in self.values:
      i = 0
      for color, val in zip(self.colors, vals):
        ypos = (self.maxval-val)*self.scale
        pygame.draw.line(self.image, color, (xpos-self.xstep, lastYPosS[i]), (xpos, ypos))
        lastYPosS[i] = ypos
        i += 1
      xpos += self.xstep

