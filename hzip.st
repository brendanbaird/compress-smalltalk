#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger "printStringRadix: 16"]
   ]
]

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.

FileStream extend [
    |bitIndex currentChar|
    nl := Character nl.

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^ bit
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]
   
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' ']. 
         stdout << "count << ': ' <<" self nextLine << nl.
      ]
   ]
]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   postorder: visitor [
      visitor value: (char asInteger).
   ]
   isLeaf [
      ^true.
   ]
]
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
Leaf subclass: Tree [
   |left right|
    left [^left]
    right [^right]
    
   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor [
        left postorder: visitor.
        right postorder: visitor.
        visitor value: (1 asString)
   ]
   
   isLeaf [
        ^false.
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

nl := Character nl.

| d t c u argIn out |
d := false. "debug"
t := false."print tree"
c := false. "compress file"
u := false. "uncompress file"

freqTable := ZeroArray new: 257.
codeTable := ZeroArray new: 257.
freqTable at: 256 put: 1. "End of File"
priority := SortedCollection new.

argIn := Smalltalk arguments at: 2.
out := stdout.

(Smalltalk arguments size = 3) ifTrue: [
   |outfile|
   outfile := Smalltalk arguments at: 3.
   out := FileStream open: outfile mode: FileStream write.
].

"flag checker"
1 to: (Smalltalk arguments size) do: [:index | 
   arg:= (Smalltalk arguments at: index).
   (index = 1) ifTrue:[
      (arg includes: $-) ifTrue: [
         (arg includes: $c) ifTrue: [c := true.].
         (arg includes: $t) ifTrue: [t := true.].
         (arg includes: $u) ifTrue: [u := true.].
         (arg includes: $d) ifTrue: [d := true.].
      ].
   ].
].

d ifTrue: [stdout<<d<<nl<<t<<nl<<c<<nl<<u<<nl.].

(c | t) ifTrue: [
   "compression"

   | whatFile nodeL nodeR tree |
   whatFile := FileStream open: argIn mode: FileStream read.

   [whatFile atEnd not] whileTrue: [
      |ordChar|
      ordChar := whatFile next asInteger.
      freqTable incr: ordChar.
   ].

   whatFile close.

   freqTable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
         |leaf|
         leaf := Leaf new: key count: value.
         priority add: leaf.
      ]
   ].

   [(priority size) > 1] whileTrue: [
      nodeL := priority removeFirst.
      nodeR := priority removeFirst.

      tree := Tree new: (nodeL char)
         count: ((nodeL count) + (nodeR count))
         left: nodeL right: nodeR.

      priority add: tree.
   ].

   tree := priority removeFirst.

   tree inorder: [:char :string | 
      codeTable at: (char asInteger) put: string.
   ] prefix: ''.

   c ifTrue: [
      "-c"
      (Smalltalk arguments size = 2) ifTrue: [
         "print to out"
         | index tmp infile count |
         count := 0.

         tree postorder: [:char |
            index := 9.
            tmp := char asInteger.
            (char = 256) ifTrue: [index := 10. tmp := 1].
            (char isString) ifTrue: [index := 1].
            (char = 0) ifTrue: [index := 10. tmp := 0].
            [index > 0] whileTrue: [
               out << (tmp bitAt: index).
               index := index - 1.
               count := count + 1.
            ].
         ].

         "prints encoded character to output"
         infile := FileStream open: argIn mode: FileStream read.
         [infile atEnd not] whileTrue: [
            | ckey outp |
            ckey := infile next asInteger.
            outp := codeTable at: ckey asInteger.
            out << outp.
            count := count + (outp asString size).
         ].
         infile close.
         tmp := codeTable at: 256 asInteger.
         count := count + (tmp asString size).
         out << tmp.
         [((count \\ 8) = 0) not] whileTrue: [
            count := count + 1.
            out << 0.
         ].
         out << nl.
      ]

   ]
   ifFalse: [
      "-t"
      tree inorder: [:char :string |
         | freqVal charForm freqForm |

         "formats the character"
         charForm := ''.
         (char < 10) ifTrue: [charForm := '  '.].
         ((char >= 10) & (char < 100)) ifTrue: [charForm := ' '.].
         (char asCharacter isGraph) ifTrue: [charForm := ' '.].

         "formats the frequency"
         freqVal := freqTable at: char.
         freqForm := ''.
         (freqVal < 10) ifTrue: [freqForm := '    '.].
         ((freqVal >= 10) & (freqVal < 100)) ifTrue: [
            freqForm := '   '.].
         ((freqVal >= 100) & (freqVal < 1000)) ifTrue: [
            freqForm := '  '.].
         ((freqVal >= 1000) & (freqVal <10000)) ifTrue: [
            freqForm := ' '.].
         
         "prints the thang"
         out << charForm << ((char asCharacter) visible) << ' '
         << freqForm << freqVal << ' ' << string << nl.
      ] prefix: ''.

   ]

].

u ifTrue: [
   "decompression"

   |isEOF send stack fileBits treeDone reconTree 
   node infile outfile leafc bit tnode specialBit|

   isEOF := false.

   infile := FileStream open: argIn mode: FileStream read.

   send := OutBits new: out.
   stack := OrderedCollection new.
   fileBits := BitStream new: infile.
   treeDone := false.
   reconTree := ''.
   node := ''.

   "bit<-input"
   [(fileBits atEnd not) & (isEOF not)] whileTrue: [
      tnode := fileBits nextBit.
      leafc := 0.

      treeDone ifFalse: [

         "reads byte into leaf and pushes to stack"
         tnode = 0 ifTrue: [
            8 to: 1 by: -1 do: [:i | 
               bit := fileBits nextBit.
               leafc := leafc bitAt: i put: bit.
            ].

            leafc = 0 ifTrue: [
               specialbit := fileBits nextBit.
               specialbit = 1 ifTrue: [
                  leafc := leafc bitAt: 9 put: 1.
               ].
            ].

            stack add: (Leaf new: leafc count: '').
         ].

         "pops 1-subtree then 0-subtree creates new 
         tree then pushes to stack"
         tnode = 1 ifTrue: [
            stack size > 1 ifTrue: [
               rightchild := stack removeLast.
               leftchild := stack removeLast.

               newtree := Tree new: -1 count: '' left: leftchild 
                  right: rightchild.

               stack add: newtree.
            ] 
            ifFalse: [
               treeDone := true.
               reconTree := stack removeLast.
               node := reconTree.
            ].
         ].
      ]

      "if tree is constructed"
      ifTrue: [
         node isLeaf ifTrue: [
            ((node char) = 256) ifTrue: [isEOF := true.]
            ifFalse: [
               out nextPut: (Character value: (node char)).
               node := reconTree.
            ].
         ]
         ifFalse: [
            tnode = 0 ifTrue: [
               node := node left.
            ]
            ifFalse: [
               node := node right.
            ].

            node isLeaf ifTrue: [
               isEOF not ifTrue: [
                  ((node char) = 256) ifTrue: [
                     isEOF := true.
                  ]
                  ifFalse: [
                     outfile = nil ifTrue: [
                        out nextPut: (Character value: (node char)).
                     ]
                     ifFalse: [
                        8 to: 1 by: -1 do: [:index |
                           bit := (node char) bitAt: index.
                           send writeBit: bit.
                        ].
                     ].
                     node := reconTree.
                  ].
               ].
            ]
         ]
      ]
   ].
].
out close.
