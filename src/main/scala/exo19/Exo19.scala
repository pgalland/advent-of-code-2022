package exo19

import scala.collection.mutable

object Exo19 {

  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo19/test.txt")
    try {
      val lines      = src.getLines()
      val blueprints = lines.map(parseLine).toSeq

      println(
        maxGeodes(
          minutesLeft = 24,
          resources = Resources(oreRobots = 1),
          blueprint = blueprints.head
        )
      )
    } finally {
      src.close()
    }
  }

  def maxGeodes(minutesLeft: Int, resources: Resources, blueprint: Blueprint): Int = {
    if (minutesLeft == 0) {
      resources.geodes
    } else {
      resources
        .allBuilds(blueprint)
        .map(buildChoice => resources.atEndOfMinute(blueprint, buildChoice))
        .map(newResources => maxGeodes(minutesLeft - 1, newResources, blueprint))
        .maxOption
        .getOrElse(0)
    }
  }

  case class Blueprint(
      number: Int,
      oreRobotCost: Int,
      clayRobotCost: Int,
      obsidianRobotCost: ObsidianRobotCost,
      geodeRobotCost: GeodeRobotCost
  )

  case class ObsidianRobotCost(ore: Int, clay: Int)
  case class GeodeRobotCost(ore: Int, obsidian: Int)
  case class Cost(ore: Int, clay: Int, obsidian: Int)

  case class Build(
      oreRobots: Int,
      clayRobots: Int,
      obsidianRobots: Int,
      geodeRobots: Int
  ) {
    def cost(blueprint: Blueprint): Cost = Cost(
      ore = oreRobots * blueprint.oreRobotCost +
        clayRobots * blueprint.clayRobotCost +
        obsidianRobots * blueprint.obsidianRobotCost.ore +
        geodeRobots * blueprint.geodeRobotCost.ore,
      clay = obsidianRobots * blueprint.obsidianRobotCost.clay,
      obsidian = geodeRobots * blueprint.geodeRobotCost.obsidian
    )
  }

  case class Resources(
      ore: Int = 0,
      clay: Int = 0,
      obsidian: Int = 0,
      geodes: Int = 0,
      oreRobots: Int = 0,
      clayRobots: Int = 0,
      obsidianRobots: Int = 0,
      geodeRobots: Int = 0
  ) {

    def isMax(build: Build, blueprint: Blueprint): Boolean = {
      val cost         = build.cost(blueprint)
      val oreLeft      = ore - cost.ore
      val clayLeft     = ore - cost.clay
      val obsidianLeft = obsidian - cost.obsidian
      blueprint.oreRobotCost > oreLeft && blueprint.clayRobotCost > oreLeft &&
      (blueprint.obsidianRobotCost.ore > oreLeft || blueprint.obsidianRobotCost.clay > clayLeft) &&
      (blueprint.geodeRobotCost.ore > oreLeft || blueprint.geodeRobotCost.obsidian > obsidianLeft)
    }

    def allBuilds(blueprint: Blueprint): Seq[Build] = {
      val nbGeodeRobots = math.min(
        obsidian / blueprint.geodeRobotCost.obsidian,
        ore / blueprint.geodeRobotCost.ore
      )
      for
        nbObsidianRobots <- 0 to math.min(
          clay / blueprint.obsidianRobotCost.clay,
          (ore - nbGeodeRobots * blueprint.geodeRobotCost.ore) / blueprint.obsidianRobotCost.ore
        )
        nbClayRobots <-
          0 to (ore - nbGeodeRobots * blueprint.geodeRobotCost.ore - nbObsidianRobots * blueprint.obsidianRobotCost.ore) / blueprint.clayRobotCost
        nbOreRobots <-
          0 to (ore - nbGeodeRobots * blueprint.geodeRobotCost.ore - nbObsidianRobots * blueprint.obsidianRobotCost.ore - nbClayRobots * blueprint.clayRobotCost) / blueprint.oreRobotCost
      yield Build(nbOreRobots, nbClayRobots, nbObsidianRobots, nbGeodeRobots)
    }

    def atEndOfMinute(
        blueprint: Blueprint,
        build: Build
    ): Resources = {
      val cost = build.cost(blueprint)
      val resources = Resources(
        ore = ore - cost.ore + oreRobots,
        clay = clay - cost.clay + clayRobots,
        obsidian = obsidian - cost.obsidian + obsidianRobots,
        geodes = geodes + geodeRobots,
        oreRobots = oreRobots + build.oreRobots,
        clayRobots = clayRobots + build.clayRobots,
        obsidianRobots = obsidianRobots + build.obsidianRobots,
        geodeRobots = geodeRobots + build.geodeRobots
      )
      assert(resources.isValid)
      resources
    }

    def isValid: Boolean = {
      ore >= 0 && clay >= 0 && obsidian >= 0 && geodes >= 0 && oreRobots >= 0 && clayRobots >= 0 && obsidianRobots >= 0 && geodeRobots >= 0
    }
  }

  def parseLine(line: String) = {
    val Seq(
      number,
      oreRobotCost,
      clayRobotCost,
      obsidianRobotCostOre,
      obsidianRobotCostClay,
      geodeRobotCostOre,
      geodeRobotCostObsidian
    ) = ("""\d+""".r findAllIn line).toSeq.map(_.toInt)

    Blueprint(
      number,
      oreRobotCost,
      clayRobotCost,
      ObsidianRobotCost(obsidianRobotCostOre, obsidianRobotCostClay),
      GeodeRobotCost(geodeRobotCostOre, geodeRobotCostObsidian)
    )
  }
}
