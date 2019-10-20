%board(Row,Column,Color) ,Color is black or white

%tool(Color,Type,Row,Col):
%  Color is the group color(w or b for white or black).
%  Type is minion or queen.
%  Row is int between 1 to size of the board
%  Col is int between 1 to size of the board

%pos(Turn,WTools,BTools):
%  Turn is b or w(for black or white).
%  WTools is the current white tools in the game
%  BTools is the current black tools in the game
:- use_module(library(jpl)).

:-dynamic(tool/4).
:-dynamic(size/1).
:-dynamic(goodPos/3).
:-dynamic(last_pos/1).
:-dynamic(lvl/1).
:-dynamic(depth/1).
:-dynamic(drawCnt/1).
:-dynamic(draw/1).
:-dynamic(toolsVal/2).
:-dynamic(moves/2).

init_drawCnt:-
    retractall(drawCnt(_)),
    assert(drawCnt(0)).


clean:-
    retractall(tool(_,_,_,_)).

init(N,Lvl):-
    set_prolog_flag(stack_limit, 6 000 000 000),
    retractall(draw(_)),
    retractall(size(_)),
    retractall(lvl(_)),
    retractall(last_pos(_)),
    retractall(toolsVal(_,_)),

    retractall(depth(_)),
    assert(depth(4)),
    retractall(goodPos(_,_,_)),
    retractall(tool(_,_,_,_)),
    init_drawCnt,
    assert(lvl(Lvl)),
    assert(size(N)),
    iniToolsVal(1),
    fillMinions(N).

test(ToPos):-
    init(8,hard),
    retractall(tool(_,_,_,_)),
    assert(tool(w,minion,4,3)),
    assert(tool(w,minion,2,3)),
    assert(tool(b,minion,5,4)),
    assert(tool(b,minion,2,7)),
    findall(tool(w,T,R,C),tool(w,T,R,C),WList),
    findall(tool(b,T1,R1,C1),tool(b,T1,R1,C1),BList),
    Pos = pos(b,WList,BList),
    comp_turn(Pos,ToPos).

test2(ToPos,Res):-
    init(8,hard),!,
    retract(tool(w,minion,3,8)),
    assert(tool(w,minion,5,4)),
    findall(tool(w,T,R,C),tool(w,T,R,C),WList),
    findall(tool(b,T1,R1,C1),tool(b,T1,R1,C1),BList),
    check_player_choise(3,2,4,1,pos(w,WList,BList),pos(b,NWList,NBList)),
    Pos = pos(b,NWList,NBList),
    comp_turn(Pos,ToPos),
    clean,
 %   ToPos = pos(w,WL,BL),
    check_player_choise(4,1,5,2,ToPos,Res),
    comp_turn(Res,_).

queenTest(P,P2):-
    init(8,hard),!,
    retract(tool(w,_,1,_)),
    retract(tool(w,_,2,3)),
    retract(tool(w,_,3,2)),
    retract(tool(b,_,6,3)),
    assert(tool(b,minion,2,3)),
    findall(tool(w,T,R,C),tool(w,T,R,C),WList),
    findall(tool(b,T1,R1,C1),tool(b,T1,R1,C1),BList),
    Pos = pos(b,WList,BList),
    comp_turn(Pos,P),
    P = pos(w,WL,BL),
    comp_turn(pos(b,WL,BL),P2).

queenTest2(P):-
    init(8,hard),!,
    retract(tool(b,_,8,1)),
    retract(tool(b,_,6,3)),
    retract(tool(w,_,1,2)),
    assert(tool(w,minion,6,3)),
    findall(tool(w,T,R,C),tool(w,T,R,C),WList),
    findall(tool(b,T1,R1,C1),tool(b,T1,R1,C1),BList),
    Pos = pos(w,WList,BList),
    check_player_choise(6,3,8,1,Pos,P).

queenTest3(P):-
    init(8,hard),!,
    retract(tool(b,_,8,1)),
    retract(tool(b,_,8,7)),
    assert(tool(w,queen,8,7)),
    retract(tool(b,_,7,2)),
    retract(tool(b,_,7,6)),
    retract(tool(w,_,1,2)),
    findall(tool(w,T,R,C),tool(w,T,R,C),WList),
    findall(tool(b,T1,R1,C1),tool(b,T1,R1,C1),BList),
    Pos = pos(w,WList,BList),
    check_player_choise(8,7,7,2,Pos,P).

playTest:-
    init(8,hard),
    findall(tool(w,T,R,C),tool(w,T,R,C),WList),
    findall(tool(b,T1,R1,C1),tool(b,T1,R1,C1),BList),
    assert(last_pos(pos(b,WList,BList))),!,
    play.

play:-
    repeat,
       last_pos(pos(C1,W,B)),
       nl,
       write(C1),
       write(" turn from position:"),nl,
       write(pos(C1,W,B)),
       comp_turn(pos(C1,W,B),ToPos),
       retract(last_pos(pos(C1,W,B))),
       color(C1,C2),
       ToPos = pos(C2,NW,NB),
       assert(last_pos(pos(C2,NW,NB))),
       (   (win(pos(C2,NW,NB)),!)
       ;   fail).

check_player_choise(FromRow,FromCol,ToRow,ToCol,pos(w,WList,BList),pos(b,NWList,NBList)):-
    integer(FromRow),
    integer(FromCol),
    integer(ToRow),
    integer(ToCol),
    bound(FromRow,FromCol),
    bound(ToRow,ToCol),
    clean,
    assertTools(WList,BList),
    Tool = tool(w,_,FromRow,FromCol),
    call(Tool),!,
    (   canMove(Tool,tool(w,Type,ToRow,ToCol)),!
    ;  (canEat(Tool,tool(w,Type,ToRow,ToCol),Eaten),
        retractList(Eaten))),
    retract(Tool),
    assert(tool(w,Type,ToRow,ToCol)),
    findall(tool(w,T,R,C),tool(w,T,R,C),NWList),
    findall(tool(b,T1,R1,C1),tool(b,T1,R1,C1),NBList),
    (  noChange(pos(w,WList,BList),pos(b,NWList,NBList)),!,
       update_drawCnt
    ;  init_drawCnt).

comp_turn(Pos,ToPos):-
    clean,
    alphabeta(Pos,-999,999,ToPos,_,0),
    nonvar(ToPos),!,
    retract(depth(D)),
    NewDepth is D+1,
    (   NewDepth <4,!,
        assert(depth(NewDepth))
    ;   assert(depth(D))),
    (  noChange(Pos,ToPos),!,
       update_drawCnt
    ;  init_drawCnt).



% The alpha-beta algorithm
alphabeta( Pos, _, _, GoodPos, Val,Depth)  :-
     Depth \= 0,
     retract(goodPos(Pos,Val,GoodPos)),
     !,
     assert(goodPos(Pos,Val,GoodPos)).

alphabeta( Pos, Alpha, Beta, GoodPos, Val,Depth):-
    depth(Lim),
    Depth<Lim,
    NewDepth is Depth+1,
    moves( Pos, PosList),%!,
    assert(moves(Pos, PosList)),
    boundedbest( PosList, Alpha, Beta, GoodPos, Val,NewDepth),!.
alphabeta( Pos, _, _, _, Val,_):-
    staticval( Pos, Val,0),!.                           % Static value of Pos

boundedbest( [Pos | PosList], Alpha, Beta, GoodPos, GoodVal,Depth):-
    alphabeta( Pos, Alpha, Beta,OtherGoodPos, Val,Depth),
    assert(goodPos(Pos,Val,OtherGoodPos)),
    goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,Depth).


goodenough( [], _, _, Pos, Val, Pos, Val,_)  :-  !.    % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val,_)  :-
  min_to_move( Pos), Val > Beta, !                   % Maximizer attained upper bound
  ;
  max_to_move( Pos), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,Depth):-
    newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds
    boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1,Depth),
    betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta)  :-
    min_to_move( Pos), Val > Alpha, !.                 % Maximizer increased lower bound

newbounds( Alpha, Beta, Pos, Val, Alpha, Val)  :-
    max_to_move( Pos), Val < Beta, !.                 % Minimizer decreased upper bound

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged

betterof( Pos, Val, _, Val1, Pos, Val)  :-        % Pos better than Pos1
    min_to_move( Pos), Val > Val1, !
    ;
    max_to_move( Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).             % Otherwise Pos1 better



staticval(pos(Turn,WList,BList),Val,_):-
    lvl(easy),!,
    (   goal(pos(Turn,WList,BList),Val),!
    ;   (
            length2(BList,BLen),
            length2(WList,WLen),
            (   (Turn = w ,!,Val is -WLen)%-BLen)
            ;   (Val is BLen)%-WLen)
        ))).

staticval(pos(Turn,WList,BList),Val,_):-
    lvl(normal),!,
    (  goal(pos(Turn,WList,BList),Val),!
    ;   (
           cnt_minions(WList,WMinionCnt),
           length2(WList,WLen),
           WQueenCnt is WLen - WMinionCnt,
           cnt_minions(BList,BMinionCnt),
           length2(BList,BLen),
           BQueenCnt is BLen - BMinionCnt,
           WVal is (3*WQueenCnt)+WMinionCnt,
           BVal is (3*BQueenCnt)+BMinionCnt,
           Val0 is BVal-WVal,
           (   (Turn = w,!,Val is -Val0)
           ;   Val is Val0)
       )).

staticval(pos(Turn,WList,BList),Val,_):-
    lvl(hard),
    (   goal(pos(Turn,WList,BList),Val),!
    ;   (
            vals_sum(WList,WSum),
            vals_sum(BList,BSum),
            length2(WList,WLen),
            length2(BList,BLen),
            WLen \= 0,
            BLen \= 0,
            Val0 is (BSum/WLen)-(WSum/BLen),
            (   (Turn = w,!,Val is -Val0)
            ;   Val is Val0)
        )).

goal(Pos,Val):-
    won(Pos,Val),!
    ;
    draw(Pos),Val is 0.

win(Pos):-
    won(Pos,_).
won(pos(b,WList,[]),-900):-
    WList \= [],!.
won(pos(w,[],BList),900):-
    BList \= [].


min_to_move(pos(w,_,_)).
max_to_move(pos(b,_,_)).


isQueen(tool(w,_,Row,_)):-
    size(Row).
isQueen(tool(b,_,1,_)).

canEat(Tool,FinalPos,EatableTools):-
  canEat(Tool,FinalPos,0,[],EatableTools).

canEat(Tool,FinalPos,EatenNum,EatableTools,Res):-
    Tool = tool(C1,_,Row,Col),
    integer(Row),
    integer(Col),
    eat(EatenNum,Tool,E,NPos),
    E = tool(C2,_,Row2,Col2),
    color(C1,C2),
    integer(Row2),
    integer(Col2),
    not(member(E,EatableTools)),
    NEatenNum is EatenNum+1,
    canEat(NPos,FinalPos,NEatenNum,[E|EatableTools],Res).
canEat(Tool,Tool,EatenNum,Res,Res):-
    EatenNum>0.

canMove(tool(C,minion,Row,Col),tool(C,Type,NRow,NCol)):-
    !,
    (   (   left_ngb(C,Row,Col,NRow,NCol),
            not(tool(_,_,NRow,NCol)))
    ;
        (   right_ngb(C,Row,Col,NRow,NCol),
            not(tool(_,_,NRow,NCol)))
    ),
    (   (isQueen(tool(C,_,NRow,NCol)),!,Type = queen)
    ;   Type = minion).



canMove(tool(Color,queen,Row,Col),tool(Color,queen,NRow,NCol)):-
    tool(Color,queen,Row,Col),
    (
         q_uLeft_ngb(Row,Col,NRow,NCol)
         ;
         q_lLeft_ngb(Row,Col,NRow,NCol)
         ;
         q_uRight_ngb(Row,Col,NRow,NCol)
         ;
         q_lRight_ngb(Row,Col,NRow,NCol)).


move(pos(C1,WTools,BTools),pos(C2,NewWTools,NBTools)):-
    color(C1,C2),
    retractall(tool(_,_,_,_)),
    assertTools(WTools,BTools),
    T = tool(C1,_,_,_),
    call(T),
    canEat(T,NT,Eaten),
    retractList(Eaten),
    retract(T),
    assert(NT),
    findall(tool(w,T1,R1,Col1),tool(w,T1,R1,Col1),NewWTools),
    findall(tool(b,T2,R2,Col2),tool(b,T2,R2,Col2),NBTools),
    retractall(tool(_,_,_,_)),
    assertTools(WTools,BTools).
move(pos(w,WTools,BTools),pos(b,NewWTools,BTools)):-
    retractall(tool(_,_,_,_)),
    assertTools(WTools,BTools),
    T = tool(w,_,_,_),
    call(T),
    canMove(T,NT),
    retract(T),
    assert(NT),
    findall(tool(w,T1,R1,Col1),tool(w,T1,R1,Col1),NewWTools),
    retractall(tool(_,_,_,_)),
    assertTools(WTools,BTools).
move(pos(b,WTools,BTools),pos(w,WTools,NBTools)):-
    retractall(tool(_,_,_,_)),
    assertTools(WTools,BTools),
    T = tool(b,_,_,_),
    call(T),
    canMove(T,NT),
    retract(T),
    assert(NT),
    findall(tool(b,T2,R2,Col2),tool(b,T2,R2,Col2),NBTools),
    retractall(tool(_,_,_,_)),
    assertTools(WTools,BTools).


moves(Pos,PosList):-
    findall(ToPos,move(Pos,ToPos),PosList).


vals_sum([],0).
vals_sum([tool(_,minion,Row,_)|Rest],Sum):-
    !,
    vals_sum(Rest,Sum0),
    toolsVal(tool(_,minion,Row,_),Val),!,
    size(S),
    Hlf is S/2,
    Sum is Sum0+Val+Hlf.

vals_sum([tool(_,queen,_,_)|Rest],Sum):-
    vals_sum(Rest,Sum0),
    toolsVal(tool(_,queen,_,_),Val),!,
    Sum is Sum0+Val.



cnt_minions([],0).
cnt_minions([tool(_,minion,_,_)|Rest],Cnt):-
    !,
    cnt_minions(Rest,Cnt0),
    Cnt is Cnt0+1.
cnt_minions([tool(_,queen,_,_)|Rest],Cnt):-
    cnt_minions(Rest,Cnt).


assertTools([],[]).
assertTools([],[B|BTools]):-
    !,
    assert(B),
    assertTools([],BTools).
assertTools([W|WTools],[]):-
    !,
    assert(W),
    assertTools(WTools,[]).
assertTools([W|WTools],[B|BTools]):-
    assert(W),assert(B),
    assertTools(WTools,BTools).


bound(Row,Col):-
     size(S),
     Row >= 1,Row =< S,
     Col >=1,Col =< S.

%right_ngb(w,+Row,+Col,-NRow,-NCol):
%  true if there is no tool in upper right box (at NRow ,NCol of the
%  board) and this box is bound to the board size.
right_ngb(w,Row,Col,NRow,NCol):-
    NRow is Row+1,
    NCol is Col+1,
    bound(NRow,NCol).
%right_ngb(b,+Row,+Col,-NRow,-NCol):
%  true if there is no tool in lower right box (at NRow ,NCol of the
%  board) and this box is bound to the board size.
right_ngb(b,Row,Col,NRow,NCol):-
    NRow is Row-1,
    NCol is Col+1,
    bound(NRow,NCol).
%left_ngb(w,+Row,+Col,-NRow,-NCol):
%  true if there is no tool in upper left box (at NRow ,NCol of the
%  board) and this box is bound to the board size.
left_ngb(w,Row,Col,NRow,NCol):-
    NRow is Row+1,
    NCol is Col-1,
    bound(NRow,NCol).
%left_ngb(b,+Row,+Col,-NRow,-NCol):
%  true if there is no tool in lower left box (at NRow ,NCol of the
%  board) and this box is bound to the board size.
left_ngb(b,Row,Col,NRow,NCol):-
    NRow is Row-1,
    NCol is Col-1,
    bound(NRow,NCol).

%
q_uLeft_ngb(Row,Col,NRow,NCol):-
    left_ngb(w,Row,Col,NRow,NCol),
    not(tool(_,_,NRow,NCol)).
q_uLeft_ngb(Row,Col,NRow,NCol):-
    left_ngb(w,Row,Col,NRow1,NCol1),
    not(tool(_,_,NRow1,NCol1)),%!,
    q_uLeft_ngb(NRow1,NCol1,NRow,NCol).
%q_uLeft_ngb(Row,Col,NRow,NCol):-
 %   q_valid(Row,Col,NRow,NCol).

q_lLeft_ngb(Row,Col,NRow,NCol):-
    left_ngb(b,Row,Col,NRow,NCol),
    not(tool(_,_,NRow,NCol)).
q_lLeft_ngb(Row,Col,NRow,NCol):-
    left_ngb(b,Row,Col,NRow1,NCol1),
    not(tool(_,_,NRow1,NCol1)),%!,
    q_lLeft_ngb(NRow1,NCol1,NRow,NCol).

q_uRight_ngb(Row,Col,NRow,NCol):-
    right_ngb(w,Row,Col,NRow,NCol),
    not(tool(_,_,NRow,NCol)).
q_uRight_ngb(Row,Col,NRow,NCol):-
    right_ngb(w,Row,Col,NRow1,NCol1),
    not(tool(_,_,NRow1,NCol1)),%!,
    q_uRight_ngb(NRow1,NCol1,NRow,NCol).


q_lRight_ngb(Row,Col,NRow,NCol):-
    right_ngb(b,Row,Col,NRow,NCol),
    not(tool(_,_,NRow,NCol)).
q_lRight_ngb(Row,Col,NRow,NCol):-
    right_ngb(b,Row,Col,NRow1,NCol1),
    not(tool(_,_,NRow1,NCol1)),%!,
    q_lRight_ngb(NRow1,NCol1,NRow,NCol).



eat(0,tool(C1,minion,Row,Col),tool(C2,Type,NRow,NCol),tool(C1,minion,TRow,TCol)):-
    color(C1,C2),
    ((  right_ngb(C1,Row,Col,NRow,NCol),
        tool(C2,Type,NRow,NCol),
        right_ngb(C1,NRow,NCol,TRow,TCol))
    ;
    (  left_ngb(C1,Row,Col,NRow,NCol),
       tool(C2,Type,NRow,NCol),
       left_ngb(C1,NRow,NCol,TRow,TCol) )),
    bound(TRow,TCol),
    not(tool(_,_,TRow,TCol)).
eat(EatenNum,tool(C1,minion,Row,Col),tool(C2,Type,NRow,NCol),tool(C1,minion,TRow,TCol)):-
    EatenNum>0,
   color(C1,C2),
   ((  right_ngb(C,Row,Col,NRow,NCol),
       tool(C2,Type,NRow,NCol),
       right_ngb(C,NRow,NCol,TRow,TCol))
   ;
   (  left_ngb(C,Row,Col,NRow,NCol),
      tool(C2,Type,NRow,NCol),
      left_ngb(C,NRow,NCol,TRow,TCol) )),
   bound(TRow,TCol),
   not(tool(_,_,TRow,TCol)).


eat(EatenNum,tool(C1,queen,Row,Col),tool(C2,Type,NRow,NCol),tool(C1,queen,TRow,TCol)):-
    EatenNum>=0,
    color(C1,C2),
    (( (q_uRight_ngb(Row,Col,NR,NC)
        ;   (true,Row=NR,Col=NC)),
        right_ngb(w,NR,NC,NRow,NCol),
        tool(C2,Type,NRow,NCol),
        q_uRight_ngb(NRow,NCol,TRow,TCol)
     );(
         (q_lRight_ngb(Row,Col,NR,NC)
         ;   (true,Row=NR,Col=NC)),
         right_ngb(b,NR,NC,NRow,NCol),
         tool(C2,Type,NRow,NCol),
         q_lRight_ngb(NRow,NCol,TRow,TCol)
     );(
           (q_uLeft_ngb(Row,Col,NR,NC)
           ;   (true,Row=NR,Col=NC)),
           left_ngb(w,NR,NC,NRow,NCol),
           tool(C2,Type,NRow,NCol),
           q_uLeft_ngb(NRow,NCol,TRow,TCol)
       );(
           (q_lLeft_ngb(Row,Col,NR,NC)
           ;   (true,Row=NR,Col=NC)),
           left_ngb(b,NR,NC,NRow,NCol),
           tool(C2,Type,NRow,NCol),
           q_lLeft_ngb(NRow,NCol,TRow,TCol)
       )),
    bound(TRow,TCol),
    not(tool(_,_,TRow,TCol)).

%sub_del(SubList,List,Res):
% return an order(by the original order in List)sub list
% in Res
sub_del(SubList,List,Res):-
    sub_del0(SubList,-1,List,Res).
sub_del0([],_,Res,Res).
sub_del0([X|SubList],LastDelInd,List,Res):-
    del(X,List,Res1,Ind),
    Ind >= LastDelInd,
    sub_del0(SubList,Ind,Res1,Res).

del(X,[X|Tail],Tail,0).
del(X,[Y|Tail1],[Y|Tail2],Ind):-
    del(X,Tail1,Tail2,Ind1),
    Ind is Ind1+1.


length2(List,Length):-
    F =..[f|List],
    functor(F,f,Length).


noChange(pos(w,WList,BList),pos(b,NWList,NBList)):-
    length2(WList,WLen),
    length2(NWList,WLen),
    length2(BList,BLen),
    length2(NBList,BLen).

update_drawCnt:-
    retract(drawCnt(Cnt)),
    NCnt is Cnt+1,
    assert(drawCnt(NCnt)).

retractList([]).
retractList([T|Terms]):-
    compound(T),
    retract(T),
    retractList(Terms).

color(b,w).
color(w,b).


iniToolsVal(Index):-
    size(Size),
    Half is Size/2,
    Index =< Half,!,
    LastInd is Size - Index + 1,
    Val is Half - Index + 1,
    assert(toolsVal(tool(_,minion,Index,_),Val)),
    assert(toolsVal(tool(_,minion,LastInd,_),Val)),
    NInd is Index+1,
    iniToolsVal(NInd).
iniToolsVal(Index):-
    size(Size),
    Half is Size/2,
    Index > Half,
    Val is Size*2,
    assert(toolsVal(tool(_,queen,_,_),Val)).

fillMinions(Size):-
    retractall(tool(_,_,_,_)),
    NSize is (Size-2)/2,
    fillMinions(w,Size,1-NSize,2),
    OSize is Size-NSize+1,
    (   isEven(OSize),!,
        fillMinions(b,Size,OSize-Size,1)
    ;   fillMinions(b,Size,OSize-Size,2)).

fillMinions(_,Size,SRow-TRow,Col):-
    SRow=:=TRow,
    Col>Size,!.
fillMinions(C,Size,SRow-TRow,Column):-
    SRow=<TRow,
    Column =<Size,!,
    assert(tool(C,minion,SRow,Column)),
    NCol is Column+2,
    fillMinions(C,Size,SRow-TRow,NCol).
fillMinions(C,Size,SRow-TRow,Column):-
    SRow=<TRow,
    Column >Size,!,
    NSRow is SRow+1,
    (   (isEven(SRow),
         fillMinions(C,Size,NSRow-TRow,2))
    ;   (not(isEven(SRow)),fillMinions(C,Size,NSRow-TRow,1))).

isEven(X):-
    integer(X),
    Tmp1 is X/2,
    Tmp2 is X//2,
    Tmp1 is Tmp2.
