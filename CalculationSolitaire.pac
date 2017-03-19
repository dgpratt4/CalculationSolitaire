| package |
package := Package name: 'CalculationSolitaire'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #CalculationSolitaireBoard;
	add: #CalculationSolitaireGame;
	add: #CardBackCell;
	add: #CardBackCellView;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'Core\Object Arts\Dolphin\Base\Dolphin';
	add: 'Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'Core\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package!

"Class Definitions"!

Object subclass: #CalculationSolitaireBoard
	instanceVariableNames: 'cards'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #CardBackCell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #CalculationSolitaireGame
	instanceVariableNames: 'cardPresenters'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #CardBackCellView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

CalculationSolitaireBoard guid: (GUID fromString: '{00C92303-50FB-41A1-BE3A-A10A0EBF5638}')!
CalculationSolitaireBoard comment: ''!
!CalculationSolitaireBoard categoriesForClass!Kernel-Objects! !
!CalculationSolitaireBoard methodsFor!

cards
	^cards!

setSize: anInteger
	cards := (1 to: anInteger squared) collect: [:each | true asValue]  

!

size 
	^cards size sqrt truncated ! !
!CalculationSolitaireBoard categoriesFor: #cards!public! !
!CalculationSolitaireBoard categoriesFor: #setSize:!public! !
!CalculationSolitaireBoard categoriesFor: #size!public! !

!CalculationSolitaireBoard class methodsFor!

defaultSize
	^10!

new	
	^self withSize: self defaultSize !

withSize: anInteger
	^super new setSize: anInteger ! !
!CalculationSolitaireBoard class categoriesFor: #defaultSize!public! !
!CalculationSolitaireBoard class categoriesFor: #new!public! !
!CalculationSolitaireBoard class categoriesFor: #withSize:!public! !

CardBackCell guid: (GUID fromString: '{CBCA0C98-3686-44C4-B186-17107C967F3B}')!
CardBackCell comment: ''!
!CardBackCell categoriesForClass!MVP-Presenters! !
!CardBackCell class methodsFor!

defaultModel
	^true asValue!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.CardBackCellView) 98 12 0 0 98 2 8 1140850688 1 416 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 16 0 0 7 0 0 0 416 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 7679 21 690 381 501 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 189 15 0 0 4 1 0 0] 98 0 690 193 193 0 27 )! !
!CardBackCell class categoriesFor: #defaultModel!public! !
!CardBackCell class categoriesFor: #resource_Default_view!public!resources-views! !

CalculationSolitaireGame guid: (GUID fromString: '{B6BBC112-D5A3-41A0-B90D-07425D5AD652}')!
CalculationSolitaireGame comment: ''!
!CalculationSolitaireGame categoriesForClass!MVP-Presenters! !
!CalculationSolitaireGame methodsFor!

clearExistingCardPresenters
	cardPresenters do: [:each | each destroy] !

createCardPresenters 
	self clearExistingCardPresenters.
	self view layoutManager rows: self model size. 
	self model cards do: 	
			 [:eachCard|
			 |cp|
			 cp := CardBackCell createIn: self on: eachCard.
			 cardPresenters add: cp]!

initialize 
	super initialize.
	cardPresenters := OrderedCollection new!

onViewOpened
	super onViewOpened.
	self createCardPresenters! !
!CalculationSolitaireGame categoriesFor: #clearExistingCardPresenters!public! !
!CalculationSolitaireGame categoriesFor: #createCardPresenters!public! !
!CalculationSolitaireGame categoriesFor: #initialize!public! !
!CalculationSolitaireGame categoriesFor: #onViewOpened!public! !

!CalculationSolitaireGame class methodsFor!

defaultModel
	^CalculationSolitaireBoard new !

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 26607617 131073 416 0 196934 1 ##(Smalltalk.RGB) 70657 0 39 0 0 0 416 656390 ##(Smalltalk.GridLayout) 3 1 1 1 234 256 98 0 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 3 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 7679 21 706 1831 1131 416 642 8 #text: 98 1 8 'Calculation Solitaire' 416 642 8 #updateMenuBar 560 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 146 18 0 0 63 2 0 0] 98 0 706 193 193 0 27 )! !
!CalculationSolitaireGame class categoriesFor: #defaultModel!public! !
!CalculationSolitaireGame class categoriesFor: #resource_Default_view!public!resources-views! !

CardBackCellView guid: (GUID fromString: '{23789A0B-8E80-4C8F-B0ED-B4325016C652}')!
CardBackCellView comment: ''!
!CardBackCellView categoriesForClass!MVP-Resources-Misc! !
!CardBackCellView methodsFor!

colorWhenOn
	^(RGB red: 244 green: 0 blue: 0)!

connectModel
	self model when: #valueChanged send: #invalidate to: self!

onLeftButtonReleased: aMouseEvent
	self presenter trigger: #cellAction!

onPaintRequired: aPaintEvent
	self model value 
		ifTrue: 
			[| cellRect canvas |
			cellRect := self clientRectangle insetBy: 5.
			canvas := aPaintEvent canvas.
			canvas fillRectangle: cellRect color: self colorWhenOn]! !
!CardBackCellView categoriesFor: #colorWhenOn!public! !
!CardBackCellView categoriesFor: #connectModel!public! !
!CardBackCellView categoriesFor: #onLeftButtonReleased:!public! !
!CardBackCellView categoriesFor: #onPaintRequired:!public! !

!CardBackCellView class methodsFor!

defaultModel
	^true asValue!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.CardBackCellView) 98 12 0 0 98 2 8 1140850688 1 416 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 16 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 416 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 1 1 738 201 201 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 100 0 0 0] 98 0 738 193 193 0 27 )! !
!CardBackCellView class categoriesFor: #defaultModel!public! !
!CardBackCellView class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

