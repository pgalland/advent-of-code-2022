package exo19

import scala.collection.mutable
import com.google.ortools.Loader
import com.google.ortools.linearsolver.MPConstraint
import com.google.ortools.linearsolver.MPObjective
import com.google.ortools.linearsolver.MPSolver
import com.google.ortools.linearsolver.MPVariable

object Exo19withSolver {

  def main(args: Array[String]): Unit = {
    Loader.loadNativeLibraries()

    val src = scala.io.Source.fromFile("src/main/scala/exo19/test.txt")
    try {
      val lines      = src.getLines()
      val blueprints = lines.map(parseLine).toSeq

      val result = solve(blueprints(1)).result

      println(result)
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

  def solve(blueprint: Blueprint, print: Boolean = false): Turns = {

    // Create the linear solver with the SCIP backend.
    val solver   = MPSolver.createSolver("SCIP")
    val infinity = java.lang.Double.POSITIVE_INFINITY
    // Make the integer non-negative variables.
    // resources at beginning of turn n
    val oreAtn      = (0 to 23).map(n => solver.makeIntVar(0.0, infinity, s"ore_at_$n"))
    val clayAtn     = (0 to 23).map(n => solver.makeIntVar(0.0, infinity, s"clay_at_$n"))
    val obsidianAtn = (0 to 23).map(n => solver.makeIntVar(0.0, infinity, s"obsidian_at_$n"))
    val geodeAtn    = (0 to 24).map(n => solver.makeIntVar(0.0, infinity, s"geode_at_$n"))
    // nb robots built during turn n
    val buildOreRobotAtn =
      (0 to 23).map(n => solver.makeIntVar(0.0, infinity, s"build_ore_robot_at_$n"))
    val buildClayRobotAtn =
      (0 to 23).map(n => solver.makeIntVar(0.0, infinity, s"build_clay_robot_at_$n"))
    val buildObsidianRobotAtn =
      (0 to 23).map(n => solver.makeIntVar(0.0, infinity, s"build_obsidian_robot_at_$n"))
    val buildGeodeRobotAtn =
      (0 to 23).map(n => solver.makeIntVar(0.0, infinity, s"build_geode_robot_at_$n"))

    // Define the constraints
    // ore constraint
    val oreC0 = solver.makeConstraint(0, 0, s"ore_contraint_at_0")
    oreC0.setCoefficient(oreAtn(0), 1)
    (1 to 23).foreach { n =>
      val c = solver.makeConstraint(1, 1, s"ore_contraint_at_$n")
      c.setCoefficient(oreAtn(n), 1)
      c.setCoefficient(oreAtn(n - 1), -1)
      // gaining ore
      (0 until n - 1).foreach(i => c.setCoefficient(buildOreRobotAtn(i), -1))
      // spending ore
      c.setCoefficient(buildOreRobotAtn(n - 1), blueprint.oreRobotCost)
      c.setCoefficient(buildClayRobotAtn(n - 1), blueprint.clayRobotCost)
      c.setCoefficient(buildObsidianRobotAtn(n - 1), blueprint.obsidianRobotCost.ore)
      c.setCoefficient(buildGeodeRobotAtn(n - 1), blueprint.geodeRobotCost.ore)
    }
    // clay constraint
    val clayC0 = solver.makeConstraint(0, 0, s"clay_contraint_at_0")
    clayC0.setCoefficient(clayAtn(0), 1)
    (1 to 23).foreach { n =>
      val c = solver.makeConstraint(0, 0, s"clay_contraint_at_$n")
      c.setCoefficient(clayAtn(n), 1)
      c.setCoefficient(clayAtn(n - 1), -1)
      // gaining clay
      (0 until n - 1).foreach(i => c.setCoefficient(buildClayRobotAtn(i), -1))
      // spending clay
      c.setCoefficient(buildObsidianRobotAtn(n - 1), blueprint.obsidianRobotCost.clay)
    }
    // obsidian constraint
    val obsiC0 = solver.makeConstraint(0, 0, s"obsidian_contraint_at_0")
    obsiC0.setCoefficient(obsidianAtn(0), 1)
    (1 to 23).foreach { n =>
      val c = solver.makeConstraint(0, 0, s"obsidian_contraint_at_$n")
      c.setCoefficient(obsidianAtn(n), 1)
      c.setCoefficient(obsidianAtn(n - 1), -1)
      // gaining obsidian
      (0 until n - 1).foreach(i => c.setCoefficient(buildObsidianRobotAtn(i), -1))
      // spending obsidian
      c.setCoefficient(buildGeodeRobotAtn(n - 1), blueprint.geodeRobotCost.obsidian)
    }
    // geode constraint
    val geodeC0 = solver.makeConstraint(0, 0, s"geode_contraint_at_0")
    geodeC0.setCoefficient(geodeAtn(0), 1)
    (1 to 24).foreach { n =>
      val c = solver.makeConstraint(0, 0, s"geode_contraint_at_$n")
      c.setCoefficient(geodeAtn(n), 1)
      c.setCoefficient(geodeAtn(n - 1), -1)
      // gaining geode
      (0 until n - 1).foreach(i => c.setCoefficient(buildGeodeRobotAtn(i), -1))
    }
    // one robot per turn constraint
    (0 to 23).foreach { n =>
      val c = solver.makeConstraint(-infinity, 1, s"build_one_robot_per_turn_contraint_at_$n")
      // building the robots
      c.setCoefficient(buildOreRobotAtn(n), 1)
      c.setCoefficient(buildClayRobotAtn(n), 1)
      c.setCoefficient(buildObsidianRobotAtn(n), 1)
      c.setCoefficient(buildGeodeRobotAtn(n), 1)
    }
    // ore for building robot constraint
    (0 to 23).foreach { n =>
      val c = solver.makeConstraint(-infinity, 0, s"robot_build_ore_contraint_at_$n")
      // building the robots
      c.setCoefficient(buildOreRobotAtn(n), blueprint.oreRobotCost)
      c.setCoefficient(buildClayRobotAtn(n), blueprint.clayRobotCost)
      c.setCoefficient(buildObsidianRobotAtn(n), blueprint.obsidianRobotCost.ore)
      c.setCoefficient(buildGeodeRobotAtn(n), blueprint.geodeRobotCost.ore)
      // must be below the ore available
      c.setCoefficient(oreAtn(n), -1)
    }
    // clay for building robot constraint
    (0 to 23).foreach { n =>
      val c = solver.makeConstraint(-infinity, 0, s"robot_build_clay_contraint_at_$n")
      // building the robots
      c.setCoefficient(buildObsidianRobotAtn(n), blueprint.obsidianRobotCost.clay)
      // must be below the clay available
      c.setCoefficient(clayAtn(n), -1)
    }
    // obsidian for building robot constraint
    (0 to 23).foreach { n =>
      val c = solver.makeConstraint(-infinity, 0, s"robot_build_obsidian_contraint_at_$n")
      // building the robots
      c.setCoefficient(buildGeodeRobotAtn(n), blueprint.geodeRobotCost.obsidian)
      // must be below the obsidian available
      c.setCoefficient(obsidianAtn(n), -1)
    }
    // Define objective function
    val objective = solver.objective
    objective.setCoefficient(geodeAtn(24), 1)
    objective.setMaximization()

    val resultStatus = solver.solve
    if (print) {
      if (resultStatus == MPSolver.ResultStatus.OPTIMAL) {
        println("Solution:")
        println("Objective value = " + objective.value)
        println("===============")
        (0 to 23).foreach { n =>
          println(s"turn ${n + 1}:")
          println(
            s"resources start of turn: ore=${oreAtn(n).solutionValue()} clay=${clayAtn(n)
                .solutionValue()} obsidian=${obsidianAtn(n).solutionValue()} geode=${geodeAtn(n).solutionValue()}"
          )
          println(
            s"robot built: ore=${buildOreRobotAtn(n).solutionValue()} clay=${buildClayRobotAtn(n)
                .solutionValue()} obsidian=${buildObsidianRobotAtn(n)
                .solutionValue()} geode=${buildGeodeRobotAtn(n).solutionValue()}"
          )
          println(
            s"total robots end of turn: ore=${buildOreRobotAtn
                .slice(0, n + 1)
                .map(_.solutionValue())
                .sum} clay=${buildClayRobotAtn.slice(0, n + 1).map(_.solutionValue()).sum} obsidian=${buildObsidianRobotAtn
                .slice(0, n + 1)
                .map(_.solutionValue())
                .sum} geode=${buildGeodeRobotAtn.slice(0, n + 1).map(_.solutionValue()).sum}"
          )
          println("===============")
        }

      } else {
        println("The problem does not have an optimal solution!")
      }
    }
    Turns(
      objective.value().toInt,
      buildOreRobotAtn.map(_.solutionValue().toInt),
      buildClayRobotAtn.map(_.solutionValue().toInt),
      buildObsidianRobotAtn.map(_.solutionValue().toInt),
      buildGeodeRobotAtn.map(_.solutionValue().toInt)
    )
  }

  case class Turns(
      result: Int,
      buildOreRobots: Seq[Int],
      buildClayRobots: Seq[Int],
      buildObsidianRobots: Seq[Int],
      buildGeodeRobots: Seq[Int]
  ) {
    def nbGeodes(blueprint: Blueprint): Int = {
      val ((_, _, _, geodes), _) = buildOreRobots
        .zip(buildClayRobots)
        .zip(buildObsidianRobots)
        .zip(buildGeodeRobots)
        .map { case (((oreRobots, clayRobots), obsidianRobots), geodeRobots) =>
          (oreRobots, clayRobots, obsidianRobots, geodeRobots)
        }
        .foldLeft((0, 0, 0, 0) -> (1, 0, 0, 0)) {
          case (
                (
                  (ore, clay, obsidians, geodes),
                  (totalOreRobots, totalClayRobots, totalObsidianRobots, totalGeodeRobots)
                ),
                (orRobots, clayRobots, obsidianRobots, geodeRobots)
              ) =>
            val oreCost =
              orRobots * blueprint.oreRobotCost + clayRobots * blueprint.clayRobotCost + obsidianRobots * blueprint.obsidianRobotCost.ore + geodeRobots * blueprint.geodeRobotCost.ore
            val clayCost     = obsidianRobots * blueprint.obsidianRobotCost.clay
            val obsidianCost = geodeRobots * blueprint.geodeRobotCost.obsidian
            assert(oreCost <= ore)
            assert(clayCost <= clay)
            assert(obsidianCost <= obsidians)

            val remainingOre       = ore - oreCost + totalOreRobots
            val remainingClay      = clay - clayCost + totalClayRobots
            val remainingObsidians = obsidians - obsidianCost + totalObsidianRobots
            val remainingGeodes    = geodes + totalGeodeRobots

            (
              remainingOre,
              remainingClay,
              remainingObsidians,
              remainingGeodes
            ) -> (totalOreRobots + orRobots, totalClayRobots + clayRobots, totalObsidianRobots + obsidianRobots, totalGeodeRobots + geodeRobots)
        }
      geodes
    }
  }
}
