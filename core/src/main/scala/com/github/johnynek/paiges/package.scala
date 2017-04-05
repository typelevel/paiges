package com.github.johnynek

package object paiges {

  type StreamTree[T] = Stream[Step[T, Chunk]]

  type DocTree = Fix[StreamTree]
}
