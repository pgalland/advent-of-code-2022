package exo19

import com.google.ortools.Loader
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

import scala.collection.mutable

object Exo19bis {

  def main(args: Array[String]): Unit = {
    Loader.loadNativeLibraries()

    val src = scala.io.Source.fromFile("src/main/scala/exo19/input.txt")
    try {
      val lines      = src.getLines()
      val blueprints = lines.map(parseLine).toSeq.slice(0, 3)

      val result = blueprints.map(solve).product

      println(result)
    } finally {
      src.close()
    }
  }

  def solve(blueprint: Blueprint): Int = {
    // Create the linear solver with the SCIP backend.
    val solver   = MPSolver.createSolver("SCIP")
    val infinity = java.lang.Double.POSITIVE_INFINITY
    // Make the integer non-negative variables.
    // resources at beginning of turn n
    val oreAtn      = (0 to 31).map(n => solver.makeIntVar(0.0, infinity, s"ore_at_$n"))
    val clayAtn     = (0 to 31).map(n => solver.makeIntVar(0.0, infinity, s"clay_at_$n"))
    val obsidianAtn = (0 to 31).map(n => solver.makeIntVar(0.0, infinity, s"obsidian_at_$n"))
    val geodeAtn    = (0 to 32).map(n => solver.makeIntVar(0.0, infinity, s"geode_at_$n"))
    // nb robots built during turn n
    val buildOreRobotAtn =
      (0 to 31).map(n => solver.makeIntVar(0.0, infinity, s"build_ore_robot_at_$n"))
    val buildClayRobotAtn =
      (0 to 31).map(n => solver.makeIntVar(0.0, infinity, s"build_clay_robot_at_$n"))
    val buildObsidianRobotAtn =
      (0 to 31).map(n => solver.makeIntVar(0.0, infinity, s"build_obsidian_robot_at_$n"))
    val buildGeodeRobotAtn =
      (0 to 31).map(n => solver.makeIntVar(0.0, infinity, s"build_geode_robot_at_$n"))

    // Define the constraints
    // ore constraint
    val oreC0 = solver.makeConstraint(0, 0, s"ore_contraint_at_0")
    oreC0.setCoefficient(oreAtn(0), 1)
    (1 to 31).foreach { n =>
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
    (1 to 31).foreach { n =>
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
    (1 to 31).foreach { n =>
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
    (1 to 32).foreach { n =>
      val c = solver.makeConstraint(0, 0, s"geode_contraint_at_$n")
      c.setCoefficient(geodeAtn(n), 1)
      c.setCoefficient(geodeAtn(n - 1), -1)
      // gaining geode
      (0 until n - 1).foreach(i => c.setCoefficient(buildGeodeRobotAtn(i), -1))
    }
    // one robot per turn constraint
    (0 to 31).foreach { n =>
      val c = solver.makeConstraint(-infinity, 1, s"build_one_robot_per_turn_contraint_at_$n")
      // building the robots
      c.setCoefficient(buildOreRobotAtn(n), 1)
      c.setCoefficient(buildClayRobotAtn(n), 1)
      c.setCoefficient(buildObsidianRobotAtn(n), 1)
      c.setCoefficient(buildGeodeRobotAtn(n), 1)
    }
    // ore for building robot constraint
    (0 to 31).foreach { n =>
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
    (0 to 31).foreach { n =>
      val c = solver.makeConstraint(-infinity, 0, s"robot_build_clay_contraint_at_$n")
      // building the robots
      c.setCoefficient(buildObsidianRobotAtn(n), blueprint.obsidianRobotCost.clay)
      // must be below the clay available
      c.setCoefficient(clayAtn(n), -1)
    }
    // obsidian for building robot constraint
    (0 to 31).foreach { n =>
      val c = solver.makeConstraint(-infinity, 0, s"robot_build_obsidian_contraint_at_$n")
      // building the robots
      c.setCoefficient(buildGeodeRobotAtn(n), blueprint.geodeRobotCost.obsidian)
      // must be below the obsidian available
      c.setCoefficient(obsidianAtn(n), -1)
    }
    // Define objective function
    val objective = solver.objective
    objective.setCoefficient(geodeAtn(32), 1)
    objective.setMaximization()
    // result
    solver.solve()
    objective.value().round.toInt
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
