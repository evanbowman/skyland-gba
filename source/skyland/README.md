# Introduction

Welcome to the SKYLAND source code! To get started, I recommend familiarizing yourself with some of the most critical classes:


## Room

The effective base class of all blocks in the game.


## Island

A glorified container of blocks, with an admittedly somewhat bloated interface of all sorts of accessors for objects on the island.


## Scene

Implements scenes in the game. A scene does not own game entities, scenes act more like adapters, implementing update logic for islands and entities.
