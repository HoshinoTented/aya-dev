// Copyright (c) 2020-2022 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.core;

import kala.collection.Seq;
import org.aya.core.def.FnDef;
import org.aya.core.visitor.MonoidalVarFolder;
import org.aya.ref.LocalVar;
import org.aya.tyck.TyckDeclTest;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class UsagesTest {
  @Test public void refFinder() {
    assertTrue(MonoidalVarFolder.RefFinder.HEADER_AND_BODY.withBody());
    TyckDeclTest.successTyckDecls("""
      open data Nat : Type 0 | zero | suc Nat
      def one : Nat => suc zero
      open data Int : Type 0 | pos Nat | neg Nat { | zero => pos zero }
      def abs (a : Int) : Nat
       | pos n => n
       | neg n => n
      open data Fin (n : Nat) : Type | suc m => fzero | suc m => fsuc (Fin m)
      """)._2.forEach(def -> {
      assertFalse(MonoidalVarFolder.RefFinder.HEADER_AND_BODY.apply(def).isEmpty());
      var of = MonoidalVarFolder.RefFinder.HEADER_ONLY.apply(def);
      if (Seq.of("Nat", "Int").contains(def.ref().name())) assertTrue(of.isEmpty());
      else assertFalse(of.isEmpty());
      if (def instanceof FnDef fn && fn.body.isLeft())
        assertEquals(0, fn.body.getLeftValue().findUsages(new LocalVar("233")));
    });
  }
}
