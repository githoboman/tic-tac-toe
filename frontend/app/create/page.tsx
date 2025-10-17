"use client";

import { GameBoard } from "@/components/game-board";
import { useStacks } from "@/hooks/use-stacks";
import { EMPTY_BOARD, Move } from "@/lib/contract";
import { formatStx, parseStx } from "@/lib/stx-utils";
import { useState, useEffect } from "react";

export default function CreateGame() {
  const { stxBalance, userData, connectWallet, handleCreateGame } = useStacks();

  const [betAmount, setBetAmount] = useState(0);
  const [board, setBoard] = useState(EMPTY_BOARD);
  const [useLending, setUseLending] = useState(false);
  
  // Calculated values
  const [collateralNeeded, setCollateralNeeded] = useState(0);
  const [interestCost, setInterestCost] = useState(0);
  const [canAfford, setCanAfford] = useState(true);

  // Calculate lending costs when bet or toggle changes
  useEffect(() => {
    if (betAmount === 0) return;
    
    const betInMicroStx = parseStx(betAmount);
    
    if (useLending) {
      // Collateral = 150% of stake (base-collateral-ratio = 15000 basis points)
      const collateral = Math.floor(betInMicroStx * 1.5);
      // Interest = 5% of stake (interest-rate = 500 basis points)
      const interest = Math.floor(betInMicroStx * 0.05);
      
      setCollateralNeeded(collateral);
      setInterestCost(interest);
      setCanAfford(stxBalance >= collateral);
    } else {
      setCollateralNeeded(0);
      setInterestCost(0);
      setCanAfford(stxBalance >= betInMicroStx);
    }
  }, [betAmount, useLending, stxBalance]);

  function onCellClick(index: number) {
    const tempBoard = [...EMPTY_BOARD];
    tempBoard[index] = Move.X;
    setBoard(tempBoard);
  }

  async function onCreateGame() {
    const moveIndex = board.findIndex((cell) => cell !== Move.EMPTY);
    
    if (moveIndex === -1) {
      window.alert("Please make your first move on the board!");
      return;
    }
    
    if (!canAfford) {
      window.alert(
        useLending 
          ? `Insufficient balance! You need ${formatStx(collateralNeeded)} STX for collateral.`
          : `Insufficient balance! You need ${betAmount} STX.`
      );
      return;
    }
    
    const move = Move.X;
    await handleCreateGame(parseStx(betAmount), moveIndex, move, useLending);
  }

  return (
    <section className="flex flex-col items-center py-20">
      <div className="text-center mb-20">
        <h1 className="text-4xl font-bold">Create Game</h1>
        <span className="text-sm text-gray-500">
          Make a bet and play your first move
        </span>
      </div>

      <div className="flex flex-col gap-4 w-[400px]">
        <GameBoard
          board={board}
          onCellClick={onCellClick}
          nextMove={Move.X}
          cellClassName="size-32 text-6xl"
        />

        {/* Bet Amount Input */}
        <div className="flex items-center gap-2 w-full">
          <span className="">Bet: </span>
          <input
            type="number"
            className="w-full rounded bg-gray-800 px-1"
            placeholder="0"
            value={betAmount}
            onChange={(e) => {
              setBetAmount(parseInt(e.target.value));
            }}
          />
          <div
            className="text-xs px-1 py-0.5 cursor-pointer hover:bg-gray-700 bg-gray-600 border border-gray-600 rounded"
            onClick={() => {
              setBetAmount(formatStx(stxBalance));
            }}
          >
            Max
          </div>
        </div>

        {/* Lending Pool Toggle */}
        <div className="border border-gray-700 rounded-lg p-4 bg-gray-900">
          <div className="flex items-center justify-between mb-3">
            <div>
              <h3 className="font-semibold">Use Lending Pool 💰</h3>
              <p className="text-xs text-gray-500">
                Borrow STX instead of staking your own
              </p>
            </div>
            <button
              onClick={() => setUseLending(!useLending)}
              className={`relative inline-flex h-6 w-11 items-center rounded-full transition-colors ${
                useLending ? "bg-blue-500" : "bg-gray-600"
              }`}
            >
              <span
                className={`inline-block h-4 w-4 transform rounded-full bg-white transition-transform ${
                  useLending ? "translate-x-6" : "translate-x-1"
                }`}
              />
            </button>
          </div>

          {/* Cost Breakdown */}
          {betAmount > 0 && (
            <div className="space-y-1 text-sm border-t border-gray-700 pt-3">
              {useLending ? (
                <>
                  <div className="flex justify-between">
                    <span className="text-gray-400">Stake (Borrowed):</span>
                    <span>{betAmount} STX</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-400">Collateral (150%):</span>
                    <span className="text-yellow-400">
                      {formatStx(collateralNeeded)} STX
                    </span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-400">Interest (5%):</span>
                    <span className="text-orange-400">
                      {formatStx(interestCost)} STX
                    </span>
                  </div>
                  <div className="border-t border-gray-700 pt-2 mt-2">
                    <div className="flex justify-between">
                      <span className="text-gray-300">If You Win:</span>
                      <span className="text-green-400 font-semibold">
                        +{betAmount - formatStx(interestCost)} STX
                      </span>
                    </div>
                    <p className="text-xs text-gray-500 mt-1">
                      (Win pot, repay loan + interest, get collateral back)
                    </p>
                  </div>
                  <div className="flex justify-between mt-2">
                    <span className="text-gray-300">If You Lose:</span>
                    <span className="text-red-400 font-semibold">
                      -{formatStx(collateralNeeded)} STX
                    </span>
                  </div>
                  <p className="text-xs text-gray-500 mt-1">
                    (Lose collateral if you dont repay loan)
                  </p>
                </>
              ) : (
                <>
                  <div className="flex justify-between">
                    <span className="text-gray-400">Your Stake:</span>
                    <span>{betAmount} STX</span>
                  </div>
                  <div className="border-t border-gray-700 pt-2 mt-2">
                    <div className="flex justify-between">
                      <span className="text-gray-300">If You Win:</span>
                      <span className="text-green-400 font-semibold">
                        +{betAmount} STX
                      </span>
                    </div>
                    <div className="flex justify-between mt-2">
                      <span className="text-gray-300">If You Lose:</span>
                      <span className="text-red-400 font-semibold">
                        -{betAmount} STX
                      </span>
                    </div>
                  </div>
                </>
              )}

              {!canAfford && (
                <div className="mt-3 p-2 bg-red-900/20 border border-red-500 rounded text-red-400 text-xs">
                  ⚠️ Insufficient balance!
                  {useLending 
                    ? ` Need ${formatStx(collateralNeeded)} STX for collateral`
                    : ` Need ${betAmount} STX`
                  }
                </div>
              )}
            </div>
          )}
        </div>

        {/* Balance Display */}
        {userData && (
          <div className="text-sm text-gray-500 text-center">
            Your Balance: {formatStx(stxBalance)} STX
          </div>
        )}

        {/* Create/Connect Button */}
        {userData ? (
          <button
            type="button"
            className={`px-4 py-2 rounded font-semibold transition-colors ${
              !canAfford || betAmount === 0
                ? "bg-gray-700 text-gray-500 cursor-not-allowed"
                : "bg-blue-500 text-white hover:bg-blue-600"
            }`}
            onClick={onCreateGame}
            disabled={!canAfford || betAmount === 0}
          >
            {useLending ? "Create Game with Loan 💸" : "Create Game"}
          </button>
        ) : (
          <button
            type="button"
            onClick={connectWallet}
            className="bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
          >
            Connect Wallet
          </button>
        )}
      </div>
    </section>
  );
}