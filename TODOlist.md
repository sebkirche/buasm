
---

## English ##

  * Move all equates to the EQUATES title
  * Move all macros to the MACROS title
  * Rename all data using the proper naming convention
  * Move them after in DATA TITLE
  * Delete long jumps to change the Macro set (which means we have to add subroutines)
  * Rewrite WNDPROC with the new programming guidelines described in Conventions/

## Français ##

> - Vérifier qu'il ne manque pas de préfixe dans le TITLE DATAS

> - Choisir un TITLE à reformater (pas à maintenir, pour le moment ;))

> - Ranger toutes les EQUATEs dans le TITLE EQUATES (essayer de les regrouper par catégories)

> - Ranger toutes MACROs dans le TITLE MACROS (essayer de les regrouper par catégories)

> - Renommer toutes les DATAs selon la convention (avant de pouvoir les ranger toutes dans le TITLE DATAS(y compris structures, tables etc.)). Les mettre au format préfixe.name préconisé et les ranger par ordre alphabétique:

> ; A:
> Prefixe.Aaxxx
> Prefixe.Abxxx
> ...

> ; B:
> Prefixe.Baxxx
> Prefixe.Bbxxx
> ...

> etc.

> - Supprimer tous les sauts longs en HLL (If While etc.) pour pouvoir changer le set de MACROS

> - Réécrire le TITLE WNDPROC avec la méthode décrite dans SAMPLE: Cela sous-entend une restructuration en sous-routines de traitement. Une fois cette décomposition faite il sera possible d'optimiser l'interface et de faire un automate pour les différents dialogues.

> - Le fait de restructurer la WNDPROPC devrait produire un nombre de TITLEs nettement plus élevé dans un premier temps ! Ensuite, une fois chacun de ces TITLE remis en forme et simplifié, nous pourrons re ventiler tout-ça correctement !