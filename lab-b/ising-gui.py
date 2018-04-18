#!/usr/bin/env python
#
# Simple frontend for the Ising model, that uses Pygame for
# visualisation.  If your Python is healthy, 'pip install pygame' (and
# maybe 'pip install numpy') should be all you need.  Maybe these
# packages are also available in your package system (they are pretty
# common).

from ising import ising

import numpy as np
import pygame
import time
import sys

width=1000
height=800

size=(width, height)
pygame.init()
pygame.display.set_caption('Ising 2D')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.Font(None, 36)

ising = ising()

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

(rngs, spins) = ising.random_grid(1337, width, height)
abs_temp = 0.2
samplerate = 0.025 # very high

def render():
    global rngs, spins
    futhark_start = time.time()
    (rngs, spins) = ising.step(abs_temp, samplerate, rngs, spins)
    ordering = ising.delta_sum(spins)
    frame = ising.render(spins).get()
    futhark_end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))
    speedmsg = "Futhark calls took %.2fms" % ((futhark_end-futhark_start)*1000)
    showText(speedmsg, (10, 10))
    tempmsg = "Temperature: %f (up/down to change)" % abs_temp
    showText(tempmsg, (10, 30))
    samplemsg = "Samplerate: %f (left/right to change)" % samplerate
    showText(samplemsg, (10, 50))
    energymsg = "Ordering: %d" % ordering
    showText(energymsg, (10, 70))
    pygame.display.flip()

pygame.key.set_repeat(500, 50)

while True:
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_UP:
                abs_temp *= 1.01
            if event.key == pygame.K_DOWN:
                abs_temp *= 0.99
            if event.key == pygame.K_LEFT:
                samplerate *= 0.99
            if event.key == pygame.K_RIGHT:
                samplerate *= 1.01
